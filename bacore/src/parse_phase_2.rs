use std::error::Error;

use itertools::Itertools;
use lisparser::data::*;

use crate::core_syntax::{
    a_fn::{AFn, ConcreteParam},
    a_struct::{AStruct, ConcreteField},
    name_mangling::{concat_overload_name, NameConcatStyle},
    spec_etc::*,
    template_fn::{Param, TemplateFn},
    template_struct::TemplateStruct,
    type_anno::{AddrMode, ConcreteTypeAnno, TemplateTypeAnno, TypeAnno},
    CompileContext,
};

use bacommon::error::*;

#[allow(unused)]
type ItemParser = fn(Box<ListData>) -> Result<(), Box<dyn Error>>;

/// One Pass for Item (Top level Define: [fn/struct/template-fn/template-struct])
pub(crate) fn parse(
    lispmodule: LispModule,
    ctx: &mut CompileContext,
) -> Result<(), Box<dyn Error>> {
    for paren_list in lispmodule.lists {
        if let ListData::NonNil(nonnil) = paren_list {
            let sym_data: SymData = nonnil.head.try_into()?;

            let dyn_parse = match sym_data.val.as_str() {
                "defn" => parse_defn,
                "def-struct" => parse_def_struct,
                "def-template-fn" => parse_def_template_fn,
                "def-template-struct" => parse_def_template_struct,
                "in-ns" => parse_in_ns,
                // "ns" => parse_ns,
                _any => {
                    unimplemented!("{}", _any)
                }
            };

            dyn_parse(ctx, nonnil.tail.clone())?
        }
    }

    Ok(())
}

fn parse_def_struct(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    let any_data_vec = (*tail).flatten();

    let name_sym: SymData = any_data_vec[0].try_into()?;
    let name = name_sym.val.to_owned();

    let fields_tuple: BracketTupleData = any_data_vec[1].try_into()?;
    let fields = parse_concrete_fields(ctx, fields_tuple)?;

    ctx.form_struct_map
        .insert(name.clone(), AStruct { name, fields });

    Ok(())
}

fn parse_concrete_params(
    ctx: &CompileContext,
    params_tuple: BracketTupleData,
) -> Result<Vec<ConcreteParam>, Box<dyn Error>> {
    let mut params = Vec::new();

    for mut param_tuple in &params_tuple.items.into_iter().chunks(2) {
        let type_anno_map: BraceMapData =
            param_tuple.next().unwrap().try_into()?;

        let type_anno = parse_concrete_type_anno_map(ctx, type_anno_map)?;

        let formal_sym: SymData = param_tuple
            .next()
            .ok_or(XXXError::new_box_err(""))?
            .try_into()?;
        let formal = formal_sym.val;

        params.push(ConcreteParam { formal, type_anno })
    }

    Ok(params)
}

#[inline]
fn parse_concrete_fields(
    ctx: &CompileContext,
    fields_tuple: BracketTupleData,
) -> Result<Vec<ConcreteField>, Box<dyn Error>> {
    parse_concrete_params(ctx, fields_tuple)
}

fn parse_concrete_type_anno_map(
    _ctx: &CompileContext,
    type_anno_map: BraceMapData,
) -> Result<ConcreteTypeAnno, Box<dyn Error>> {
    let addr = if let Some(addr_any) = type_anno_map.get_by_keyword(":addr") {
        let addr_any: SymData = addr_any.try_into()?;

        match addr_any.val.as_str() {
            "ptr" => AddrMode::Ptr,
            _ => return Err(XXXError::new_box_err("Unknown Address Mode")),
        }
    } else {
        AddrMode::Value
    };

    Ok(
        if let Some(type_any) = type_anno_map.get_by_keyword(":type-primitive")
        {
            let type_sym: SymData = type_any.try_into()?;

            if !is_primitive_type(&type_sym.val) {
                return Err(XXXError::new_box_err(&type_sym.val));
            }

            ConcreteTypeAnno::Primitive(type_sym.val.clone(), addr)
        } else if let Some(type_any) =
            type_anno_map.get_by_keyword(":type-struct")
        {
            let type_sym: SymData = type_any.try_into()?;

            ConcreteTypeAnno::Struct(type_sym.val.to_owned(), addr)
        } else {
            unreachable!("{:#?}", type_anno_map)
        },
    )
}


fn parse_defn(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    let any_data_vec = (*tail).flatten();

    let pure_name_sym: SymData = any_data_vec[0].try_into()?;
    let pure_name = pure_name_sym.val.to_owned();

    let params_tuple: BracketTupleData = any_data_vec[1].try_into()?;
    let params = parse_concrete_fields(ctx, params_tuple)?;

    let name = concat_overload_name(
        &pure_name,
        &params
            .iter()
            .map(|param| param.type_anno.clone())
            .collect_vec()[..],
        NameConcatStyle::Fn,
    );

    let ret = if let Ok(ret_map) = any_data_vec[2].try_into() {
        Some(parse_concrete_type_anno_map(ctx, ret_map)?)
    } else {
        None
    };

    let body_list: ListData = any_data_vec[3].try_into()?;

    ctx.form_fn_map.insert(
        name.clone(),
        AFn {
            name,
            params,
            ret,
            body: body_list,
        },
    );

    Ok(())
}


fn parse_def_template_struct(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    let any_data_vec = (*tail).flatten();

    let name_sym: SymData = any_data_vec[0].try_into()?;
    let name = name_sym.val.to_owned();

    let generic_tuple: BracketTupleData = any_data_vec[1].try_into()?;
    let generic_strs = parse_generic_name(generic_tuple)?;

    let fields_tuple: BracketTupleData = any_data_vec[2].try_into()?;
    let fields = parse_fields(ctx, fields_tuple, &generic_strs[..])?;

    ctx.template_struct_map.insert(
        name.clone(),
        TemplateStruct {
            name,
            fields,
            generic_placeholder_num: generic_strs.len(),
        },
    );

    Ok(())
}

fn parse_generic_name(
    generic_tuple: BracketTupleData,
) -> Result<Vec<String>, Box<dyn Error>> {
    let mut generic_strs = vec![];

    for generic_any in generic_tuple.items.into_iter() {
        let generic_sym: SymData = generic_any.try_into()?;
        generic_strs.push(generic_sym.val.to_owned())
    }

    Ok(generic_strs)
}


fn parse_def_template_fn(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    let mut any_data_iter = (*tail).flatten().into_iter();

    let name_sym: SymData = any_data_iter.next().unwrap().try_into()?;
    let name = name_sym.val.to_owned();

    let generic_tuple: BracketTupleData =
        any_data_iter.next().unwrap().try_into()?;
    let generic_strs = parse_generic_name(generic_tuple)?;

    let params_tuple: BracketTupleData =
        any_data_iter.next().unwrap().try_into()?;
    let params = parse_params(ctx, params_tuple, &generic_strs[..])?;

    let ret = if let Ok(ret_map) = any_data_iter.next().unwrap().try_into() {
        Some(parse_type_anno_map(ctx, ret_map, &generic_strs[..])?)
    } else {
        None
    };

    let body_list: ListData = any_data_iter.next().unwrap().try_into()?;

    ctx.template_fn_map.insert(
        name.clone(),
        TemplateFn {
            name,
            params,
            ret,
            body: body_list,
            generic_placeholder_num: generic_strs.len(),
        },
    );

    Ok(())
}


fn parse_type_anno_map(
    _ctx: &CompileContext,
    type_anno_map: BraceMapData,
    templates_name: &[String],
) -> Result<TypeAnno, Box<dyn Error>> {
    let addr = if let Some(addr_any) = type_anno_map.get_by_keyword(":addr") {
        let addr_any: SymData = addr_any.try_into()?;

        match addr_any.val.as_str() {
            "ptr" => AddrMode::Ptr,
            _ => return Err(XXXError::new_box_err("Unknown Address Mode")),
        }
    } else {
        AddrMode::Value
    };

    Ok(
        if let Some(type_any) = type_anno_map.get_by_keyword(":type-primitive")
        {
            let type_sym: SymData = type_any.try_into()?;

            if !is_primitive_type(&type_sym.val) {
                return Err(XXXError::new_box_err(&type_sym.val));
            }

            TypeAnno::Concrete(ConcreteTypeAnno::Primitive(
                type_sym.val.clone(),
                addr,
            ))
        } else if let Some(type_any) =
            type_anno_map.get_by_keyword(":type-struct")
        {
            let type_sym: SymData = type_any.try_into()?;

            if let Some(generic_any) = type_anno_map.get_by_keyword(":generic")
            {
                let generic_tuple: BracketTupleData =
                    generic_any.clone().try_into()?;

                let mut generic_idxs = vec![];

                for generic_any in generic_tuple.items.into_iter() {
                    let generic_sym: SymData = generic_any.try_into()?;
                    if let Some((idx, _)) = templates_name
                        .iter()
                        .find_position(|&name| name == &generic_sym.val)
                    {
                        generic_idxs.push(idx);
                    } else {
                        return Err(XXXError::new_box_err(
                            generic_sym.val.as_str(),
                        ));
                    }
                }

                TypeAnno::Template(TemplateTypeAnno::TemplateStruct(
                    type_sym.val.to_owned(),
                    generic_idxs,
                    addr,
                ))
            } else {
                TypeAnno::Concrete(ConcreteTypeAnno::Struct(
                    type_sym.val.to_owned(),
                    addr,
                ))
            }
        } else if let Some(type_any) =
            type_anno_map.get_by_keyword(":type-template")
        {
            let type_sym: SymData = type_any.try_into()?;

            if let Some((idx, _)) = templates_name
                .iter()
                .find_position(|&name| name == &type_sym.val)
            {
                TypeAnno::Template(TemplateTypeAnno::Itself(idx, addr))
            } else {
                return Err(UnknownGenericNameError::new_box_err(
                    type_sym.val.as_str(),
                ));
            }
        } else {
            unreachable!("{:#?}", type_anno_map)
        },
    )
}

fn parse_params(
    ctx: &CompileContext,
    params_tuple: BracketTupleData,
    templates_name: &[String],
) -> Result<Vec<Param>, Box<dyn Error>> {
    let mut params = Vec::new();

    for mut param_tuple in &params_tuple.items.into_iter().chunks(2) {
        let type_anno_map: BraceMapData =
            param_tuple.next().unwrap().try_into()?;

        let type_anno =
            parse_type_anno_map(ctx, type_anno_map, templates_name)?;

        let formal_sym: SymData = param_tuple
            .next()
            .ok_or(XXXError::new_box_err(""))?
            .try_into()?;
        let formal = formal_sym.val;

        params.push(Param { formal, type_anno })
    }

    Ok(params)
}

fn parse_fields(
    ctx: &CompileContext,
    params_tuple: BracketTupleData,
    templates_name: &[String],
) -> Result<Vec<Param>, Box<dyn Error>> {
    parse_params(ctx, params_tuple, templates_name)
}

#[ignore = "just skip"]
#[allow(unused)]
fn parse_ns(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    Ok(())
}

#[ignore = "just skip"]
#[allow(unused)]
fn parse_in_ns(
    ctx: &mut CompileContext,
    tail: Box<ListData>,
) -> Result<(), Box<dyn Error>> {
    Ok(())
}
