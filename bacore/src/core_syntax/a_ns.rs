use std::{
    collections::HashSet,
    error::Error,
    marker::PhantomData,
    path::{Path, PathBuf},
    rc::Rc,
};

use bacommon::{
    config::CompilerConfig, target_generator::TargetGenerator,
    vmbuilder::builder_position_at_before,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::FunctionValue,
    AddressSpace,
};
use itertools::Itertools;
use lisparser::{data::*, parser::LispParser};
use m6stack::AnIteratorWrapper;

use crate::{
    core_syntax::{
        name_mangling::{concat_overload_name, NameConcatStyle},
        type_anno::{AddrMode, ConcreteTypeAnno},
    },
    parse_phase_2,
};

use super::{
    a_fn::AFn,
    a_struct::AStruct,
    a_value::AValue,
    name_mangling::rename_fn_alias,
    spec_etc::{extract_index_path_from_dir, name_to_path},
    CompileContext, LocalContext, LocalContextType,
};
use bacommon::error::*;

////////////////////////////////////////////////////////////////////////////////
//// `NS` Form

pub struct ANS<'ctx> {
    pub name: String,             // namespace name
    pub(crate) mods: Vec<String>, // modules name belongs to the namespace
    pub(crate) path: PathBuf,
    pub(crate) ctx: CompileContext<'ctx>,
}

impl<'ctx> ANS<'ctx> {
    pub fn init(
        dir: &Path,
        vmctx: &'ctx Context,
    ) -> Result<Self, Box<dyn Error>> {
        let dirname = dir.file_name().unwrap().to_str().unwrap();
        let ctx = CompileContext::new(dirname, vmctx);
        let index_path = extract_index_path_from_dir(dir);

        let mut parser = LispParser::new(&index_path)?;
        let lm = parser.parse()?;

        let list1st: NonNilListData = lm.lists[0].clone().try_into()?;

        let head_sym: SymData = list1st.head.try_into()?;

        Ok(match head_sym.val.as_str() {
            "ns" => {
                let tail: NonNilListData = list1st.tail.try_into()?;

                let mut tail_iter = tail.flatten().into_iter();

                let ns_name_sym: SymData = tail_iter
                    .next()
                    .ok_or(XXXError::new_box_err(""))?
                    .try_into()?;
                let name = ns_name_sym.val;

                let sub_any =
                    tail_iter.next().ok_or(XXXError::new_box_err(""))?;

                let sub_tuple: BracketTupleData = sub_any.try_into()?;
                let mut mods = vec![];

                for item_any in sub_tuple.items.iter() {
                    mods.push(TryInto::<SymData>::try_into(item_any)?.val)
                }

                ANS {
                    name,
                    mods,
                    path: dir.to_owned(),
                    ctx,
                }
            }
            other => return Err(XXXError::new_box_err(other)),
        })
    }

    pub(crate) fn sub_paths(&self) -> AnIteratorWrapper<PathBuf> {
        let iter = box self.mods.iter().map(|sub| {
            let sub_name = name_to_path(sub);
            self.path.join(sub_name)
        });

        AnIteratorWrapper { iter }
    }

    pub(crate) fn load(&mut self) -> Result<(), Box<dyn Error>> {
        for sub in self.sub_paths().collect_vec().iter() {
            load_file(&sub, &mut self.ctx)?;
        }

        self.compile_ns()?;

        Ok(())
    }

    pub fn print(
        &self,
        config: &CompilerConfig,
    ) -> Result<(), Box<dyn Error>> {
        let gen = TargetGenerator::new(&self.ctx.vmmod, config);

        gen.print()
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// `Compile`

    fn compile_ns(&mut self) -> Result<(), Box<dyn Error>> {
        // Handle Function
        let fns = self.ctx.form_fn_map.values().cloned().collect_vec();

        for afn in fns.iter() {
            self.compile_declare(&afn, None)?;
        }

        for afn in fns.iter() {
            self.compile_definition(&afn)?;
        }

        // Handle Structure
        let structs = self.ctx.form_struct_map.values().cloned().collect_vec();

        for astruct in structs.iter() {
            astruct.do_circular_dependency_check(
                &self.ctx,
                &mut HashSet::new(),
            )?;
        }

        Ok(())
    }

    pub fn compile_struct(
        &mut self,
        astruct: &AStruct,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        if let Some(astruct_t) = self.ctx.struct_map.get(&astruct.name) {
            return Ok(astruct_t.clone().into());
        }

        let field_types =
        self.compile_type_anno_batch(
            &astruct
            .fields
            .iter()
            .map(|field| field.type_anno.clone())
            .collect_vec()
            [..]
        )?;

        let struct_t = self.ctx.vmctx.struct_type(&field_types[..], false);

        self.ctx
            .struct_map
            .insert(self.name.to_owned(), struct_t.clone());

        Ok(struct_t.into())
    }

    pub fn compile_type_anno(
        &mut self,
        type_anno: &ConcreteTypeAnno,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        Ok(match &type_anno {
            ConcreteTypeAnno::Primitive(ref name, addr_mode) => {
                match name.as_str() {
                    "usize" | "i64" | "u64" => {
                        let val_t = self.ctx.vmctx.i64_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    "i32" | "u32" => {
                        let val_t = self.ctx.vmctx.i32_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    "i8" | "u8" => {
                        let val_t = self.ctx.vmctx.i8_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    "float" | "f64" => {
                        let val_t = self.ctx.vmctx.f64_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ConcreteTypeAnno::Struct(name, addr_mode) => {
                if let Some(vm_t) = self.ctx.struct_map.get(name) {
                    if let AddrMode::Ptr = addr_mode {
                        vm_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        vm_t.clone().into()
                    }
                } else {
                    // collect all form-struct before compile
                    if let Some(form_struct) =
                        self.ctx.form_struct_map.get(name.as_str()).cloned()
                    {
                        // circular dependency check before that
                        let struct_t = self
                            .compile_struct(&form_struct)?
                            .into_struct_type();

                        if let AddrMode::Ptr = addr_mode {
                            struct_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            struct_t.into()
                        }
                    } else {
                        return Err(UnknownDefineError::new_box_err(name));
                    }
                }
            }
        })
    }

    pub(crate) fn compile_type_anno_batch(
        &mut self,
        types_anno: &[ConcreteTypeAnno],
    ) -> Result<Vec<BasicTypeEnum<'ctx>>, Box<dyn Error>> {
        let mut vm_types = Vec::with_capacity(types_anno.len());

        for type_anno in types_anno.iter() {
            vm_types.push(self.compile_type_anno(type_anno)?)
        }

        Ok(vm_types)
    }

    /// Compile Function Declare
    pub(crate) fn compile_declare(
        &mut self,
        afn: &AFn,
        linkage: Option<Linkage>,
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        if let Some(fn_val) = self.ctx.vmmod.get_function(&afn.vm_name()) {
            return Ok(fn_val);
        }

        let mut param_types = Vec::with_capacity(afn.params.len());

        for param in afn.params.iter() {
            param_types.push(self.compile_type_anno(&param.type_anno)?.into())
        }

        let fn_t = match &afn.ret {
            Some(type_anno) => {
                let ret_t = self.compile_type_anno(type_anno)?;

                ret_t.fn_type(&param_types[..], false)
            }
            None => {
                let ret_t = self.ctx.vmctx.void_type();

                ret_t.fn_type(&param_types[..], false)
            }
        };

        let fn_val =
            self.ctx
                .vmmod
                .add_function(&afn.vm_name(), fn_t.clone(), linkage);

        self.ctx.fn_map.insert(afn.vm_name(), fn_t.clone());

        Ok(fn_val)
    }

    /// Compile Function Definition
    pub(crate) fn compile_definition(
        &mut self,
        afn: &AFn,
    ) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
        let fn_val = self.ctx.vmmod.get_function(&afn.vm_name()).unwrap();
        let fn_blk = self.ctx.vmctx.append_basic_block(fn_val, "");
        let lctx = self.ctx.fn_top_lctx(&afn.params[..], fn_val, fn_blk);
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let ret = match &afn.body {
            ListData::Nil(_) => {
                if let Some(ref type_anno) = afn.ret {
                    return Err(MissMatchedRetTyError::new_box_err(
                        &type_anno.to_string(),
                    ));
                } else {
                    builder.build_return(None);
                    None
                }
            }
            ListData::NonNil(nonnillist) => {
                self.compile_form(lctx, nonnillist.clone())?
            }
        };

        Ok(ret)
    }

    pub(crate) fn compile_form(
        &mut self,
        lctx: LocalContext<'ctx>,
        nonnillist: NonNilListData,
    ) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
        let head_sym: SymData = nonnillist.head.try_into()?;

        let form_name = rename_fn_alias(&head_sym.val);

        let dyn_compile = match form_name.as_str() {
            "attr" => Self::compile_form_attr,
            _ => Self::compile_norm_form,
        };

        let ret = dyn_compile(self, lctx, &form_name, nonnillist.tail.clone())?;

        Ok(ret)
    }

    fn type_anno_try_into_form_struct(
        &self,
        type_anno: &ConcreteTypeAnno,
    ) -> Result<AStruct, Box<dyn Error>> {
        if let ConcreteTypeAnno::Struct(name, addr_mode) = type_anno {
            if let AddrMode::Value = addr_mode {
                if let Some(astruct) = self.ctx.form_struct_map.get(name) {
                    return Ok(astruct.clone());
                }
            }
        }

        Err(XXXError::new_box_err(type_anno.to_string().as_str()))
    }

    /// (attr sym sym)
    pub(crate) fn compile_form_attr(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
        // let lctx = self.ctx.get_lctx();
        let builder = lctx.get_builder(&self.ctx.vmctx);
        let rems = tail.flatten();

        let var_name_sym: SymData = rems[0].try_into()?;
        let var_name = &var_name_sym.val;

        let field_name_sym: SymData = rems[1].try_into()?;
        let field_name = &field_name_sym.val;

        if let Some(avalue) = self.ctx.get_avalue(&lctx, var_name) {
            let astruct =
                self.type_anno_try_into_form_struct(&avalue.type_anno)?;

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

    pub(crate) fn compile_norm_form(
        &mut self,
        lctx: LocalContext<'ctx>,
        name: &str,
        tail: Box<ListData>,
    ) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let mut avalues = vec![];

        for arg in tail.flatten() {
            let avalue = if let Some(avalue) = match &**arg {
                AnyData::Pri(pri) => Some(match pri {
                    PriData::Lit(lit) => match lit {
                        LitData::Int(int) => {
                            let (vm_val, type_anno) = if int.len == 8 {
                                let vm_val = self
                                    .ctx
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
                                let vm_val = self
                                    .ctx
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
                                let vm_val = self
                                    .ctx
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
                            let vm_val = self
                                .ctx
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
                            // let vm_val = self.ctx.vmctx.const_string(str.val.as_bytes(), true).into();
                            // let type_anno = ConcreteTypeAnno::Primitive("str".to_owned(), AddrMode::Value);

                            // AValue { type_anno, vm_val }
                            unimplemented!()
                        }
                    },
                    PriData::Sym(sym) => {
                        if let Some(avalue) =
                            self.ctx.get_avalue(&lctx, &sym.val)
                        {
                            avalue.clone()
                        } else {
                            return Err(UnknownVarError::new_box_err(
                                &sym.val,
                            ));
                        }
                    }
                    _ => unimplemented!(),
                }),
                AnyData::Agg(agg) => match agg {
                    AggData::List(list) => match list {
                        ListData::Nil(_) => None,
                        ListData::NonNil(nonnillist) => self
                            .compile_form(lctx.clone(), nonnillist.clone())?,
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
            .map(|avalue| (*avalue).type_anno.clone())
            .collect_vec();
        let args = avalues
            .iter()
            .map(|avalue| avalue.vm_val.clone().into())
            .collect_vec();

        let concrete_name = concat_overload_name(
            name,
            &concrete_types[..],
            NameConcatStyle::Fn,
        );

        let fnval =
            if let Some(fnval) = self.ctx.vmmod.get_function(&concrete_name) {
                fnval
            } else if let Some(template_fn) =
                self.ctx.template_fn_map.get(name).cloned()
            {
                let afn =
                    template_fn.expand(&mut self.ctx, &concrete_types[..])?;
                self.compile_declare(&afn, None)?;
                self.compile_definition(&afn)?;

                self.ctx.vmmod.get_function(&concrete_name).unwrap()
            } else {
                return Err(UnknownFunctionError::new_box_err(&concrete_name));
            };

        let ret = builder.build_call(fnval, &args[..], "");

        let afn = self.ctx.form_fn_map.get(&concrete_name).unwrap();

        let ret = match afn.ret {
            Some(ref type_anno) => Some(AValue {
                type_anno: type_anno.clone(),
                vm_val: ret.try_as_basic_value().unwrap_left().clone(),
            }),
            None => None,
        };

        Ok(ret)
    }
}

/// Load recursively
fn load_file(
    path: &Path,
    ctx: &mut CompileContext,
) -> Result<(), Box<dyn Error>> {
    let mut parser = LispParser::new(path)?;
    let lm = parser.parse()?;

    parse_phase_2::parse(lm, ctx)?;

    Ok(())
}
