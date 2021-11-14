use std::error::Error;

use inkwell::{builder::Builder, values::BasicValueEnum};
use lisparser::data::*;

use super::{CompileContext, a_fn::LocalContext, a_value::AValue, type_anno::ConcreteTypeAnno};
use crate::{core_syntax::name_mangling::rename_fn_alias};
use bacommon::error::*;



pub(crate) fn handle_form<'ctx>(
    cctx: &mut CompileContext<'ctx>,
    lctx: &LocalContext<'ctx>,
    builder: Builder<'ctx>,
    list: Box<ListData>,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {

    if list.is_nil() {
        return Ok(None)
    }

    let nonlist: NonNilListData = list.try_into().unwrap();

    let head_sym: SymData = nonlist.head.try_into()?;

    let form_name = rename_fn_alias(&head_sym.val);

    let dyn_form_handle = match form_name.as_str() {
        "attr" => {
            handle_spl_form_attr
        }
        _ => {
            handle_norm_form_attr
        }
    };


    let _form_res = dyn_form_handle(
        cctx,
        lctx,
        builder,
        nonlist.tail.clone(),
    )?;

    todo!()
}



/// (attr sym sym)
pub(crate) fn handle_spl_form_attr<'ctx>(
    cctx: &mut CompileContext<'ctx>,
    lctx: &LocalContext<'ctx>,
    builder: Builder<'ctx>,
    tail: Box<ListData>,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
    let rems = tail.flatten();

    let var_name_sym: SymData = rems[0].try_into()?;
    let var_name = &var_name_sym.val;

    let field_name_sym: SymData = rems[1].try_into()?;
    let field_name = &field_name_sym.val;

    if let Some(avalue) = lctx.binds.get(var_name) {
        let astruct = avalue.type_anno.try_into_form_struct(cctx)?;

        if let Some(field_index) = astruct.index_of_field(field_name) {
            let vm_val = builder.build_extract_value(
                avalue.vm_val.into_struct_value(),
                field_index as u32,
                "",
            ).unwrap();

            let type_anno = astruct.fields[field_index].type_anno.clone();

            return Ok(Some(AValue { type_anno, vm_val }))
        }
    }

    Err(XXXError::new_box_err(""))
}



pub(crate) fn handle_norm_form_attr<'ctx>(
    _cctx: &mut CompileContext<'ctx>,
    _lctx: &LocalContext<'ctx>,
    _builder: Builder<'ctx>,
    _tail: Box<ListData>,
) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {



    todo!()
}


