use std::{error::Error, marker::PhantomData};

use inkwell::{builder::Builder, values::BasicValueEnum};
use itertools::Itertools;
use lisparser::data::*;

use super::{
    LocalContext, a_value::AValue, type_anno::ConcreteTypeAnno,
    CompileContext,
};
use crate::core_syntax::{
    name_mangling::{concat_overload_name, rename_fn_alias, NameConcatStyle},
    type_anno::{self, AddrMode},
};
use bacommon::error::*;

pub(crate) fn compile_form<'ctx>(
    cctx: &mut CompileContext<'ctx>,
    _marker: &'ctx PhantomData<&'ctx ()>,
    lctx: &'ctx LocalContext<'ctx>,
    nonnillist: NonNilListData,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
    let head_sym: SymData = nonnillist.head.try_into()?;

    let form_name = rename_fn_alias(&head_sym.val);

    let dyn_compile = match form_name.as_str() {
        "attr" => compile_form_attr,
        _ => compile_norm_form,
    };

    let ret = dyn_compile(cctx,  _marker, lctx, &form_name, nonnillist.tail.clone())?;

    Ok(ret)
}


/// (attr sym sym)
pub(crate) fn compile_form_attr<'ctx>(
    cctx: &mut CompileContext<'ctx>,
    _marker: &'ctx PhantomData<&'ctx ()>,
    lctx: &'ctx LocalContext<'ctx>,
    _name: &str,
    tail: Box<ListData>,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
    // let lctx = cctx.get_lctx();
    let builder = lctx.get_builder(&cctx.vmctx);
    let rems = tail.flatten();

    let var_name_sym: SymData = rems[0].try_into()?;
    let var_name = &var_name_sym.val;

    let field_name_sym: SymData = rems[1].try_into()?;
    let field_name = &field_name_sym.val;

    if let Some(avalue) = lctx.get(var_name) {
        let astruct = avalue.type_anno.try_into_form_struct(cctx)?;

        if let Some(field_index) = astruct.index_of_field(field_name) {
            let vm_val = builder
                .build_extract_value(
                    avalue.vm_val.into_struct_value(),
                    field_index as u32,
                    "",
                )
                .unwrap();

            let type_anno = astruct.fields[field_index].type_anno.clone();

            return Ok(Some(AValue { type_anno, vm_val }));
        }
    }

    Err(XXXError::new_box_err(""))
}

pub(crate) fn compile_norm_form<'ctx>(
    cctx: &'ctx mut CompileContext<'ctx>,
    _marker: &'ctx PhantomData<&'ctx ()>,
    lctx: &'ctx LocalContext<'ctx>,
    name: &str,
    tail: Box<ListData>,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
    let builder = lctx.get_builder(&cctx.vmctx);
    let mut avalues = vec![];

    for arg in tail.flatten() {
        let avalue = if let Some(avalue) = match &**arg {
            AnyData::Pri(pri) => Some(match pri {
                PriData::Lit(lit) => match lit {
                    LitData::Int(int) => {
                        let (vm_val, type_anno) = if int.len == 8 {
                            let vm_val = cctx
                                .vmctx
                                .i64_type()
                                .const_int(int.val as u64, int.signed)
                                .into();
                            let type_anno_string =
                                if int.signed { "i64" } else { "u64" }
                                    .to_owned();

                            let type_anno = ConcreteTypeAnno::Primitive(
                                type_anno_string,
                                AddrMode::Value,
                            );

                            (vm_val, type_anno)
                        } else if int.len == 4 {
                            let vm_val = cctx
                                .vmctx
                                .i64_type()
                                .const_int(int.val as u64, int.signed)
                                .into();
                            let type_anno_string =
                                if int.signed { "i32" } else { "u32" }
                                    .to_owned();

                            let type_anno = ConcreteTypeAnno::Primitive(
                                type_anno_string,
                                AddrMode::Value,
                            );
                            (vm_val, type_anno)
                        } else if int.len == 1 {
                            let vm_val = cctx
                                .vmctx
                                .i64_type()
                                .const_int(int.val as u64, int.signed)
                                .into();
                            let type_anno_string =
                                if int.signed { "i8" } else { "u8" }
                                    .to_owned();

                            let type_anno = ConcreteTypeAnno::Primitive(
                                type_anno_string,
                                AddrMode::Value,
                            );
                            (vm_val, type_anno)
                        } else {
                            unimplemented!()
                        };

                        AValue { type_anno, vm_val }
                    }
                    LitData::Float(float) => {
                        let vm_val = cctx
                            .vmctx
                            .f64_type()
                            .const_float(float.val)
                            .into();
                        let type_anno = ConcreteTypeAnno::Primitive(
                            "float".to_owned(),
                            AddrMode::Value,
                        );

                        AValue { type_anno, vm_val }
                    }
                    LitData::Str(_str) => {
                        // let vm_val = cctx.vmctx.const_string(str.val.as_bytes(), true).into();
                        // let type_anno = ConcreteTypeAnno::Primitive("str".to_owned(), AddrMode::Value);

                        // AValue { type_anno, vm_val }
                        unimplemented!()
                    }
                },
                PriData::Sym(sym) => {
                    if let Some(avalue) = lctx.get(&sym.val).cloned() {
                        avalue
                    }
                    else {
                        return Err(UnknownVarError::new_box_err(&sym.val))
                    }
                }
                _ => unimplemented!(),
            }),
            AnyData::Agg(agg) => match agg {
                AggData::List(list) => match list {
                    ListData::Nil(_) => None,
                    ListData::NonNil(nonnillist) => {
                        compile_form(cctx, _marker, lctx, nonnillist.clone())?
                    }
                },
                _ => unimplemented!(),
            },
        } {
            avalue
        } else {
            return Err(VoidAsArgError::new_box_err(
                format!("{:#?}", arg).as_str(),
            ));
        };

        avalues.push(avalue);
    }

    let concrete_types = avalues
        .iter()
        .map(|avalue| avalue.type_anno.clone())
        .collect_vec();
    let args = avalues
        .iter()
        .map(|avalue| avalue.vm_val.clone().into())
        .collect_vec();

    let concrete_name =
        concat_overload_name(name, &concrete_types[..], NameConcatStyle::Fn);

    let fnval = if let Some(fnval) = cctx.vmmod.get_function(&concrete_name) {
        fnval
    } else if let Some(template_fn) = cctx.template_fn_map.get(name).cloned() {
        let afn = template_fn.expand(cctx, &concrete_types[..])?;
        afn.compile_declare(cctx)?;
        afn.compile_definition(cctx, _marker)?;

        cctx.vmmod.get_function(&concrete_name).unwrap()
    } else {
        return Err(UnknownFunctionError::new_box_err(&concrete_name));
    };

    let ret = builder.build_call(fnval, &args[..], "");

    let afn = cctx.form_fn_map.get(&concrete_name).unwrap();

    Ok(match afn.ret {
        Some(ref type_anno) => {
            Some(AValue {
                type_anno: type_anno.clone(),
                vm_val: ret.try_as_basic_value().unwrap_left().clone(),
            })
        }
        None => None,
    })
}
