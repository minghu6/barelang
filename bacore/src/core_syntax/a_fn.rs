use std::{error::Error, marker::PhantomData, rc::Rc};

use convert_case::{Case, Casing};
use indexmap::IndexMap;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Linkage,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::InstructionValue,
};
use itertools::Itertools;
use lisparser::{data::*, pretty_printer::Dump};

use super::{CompileContext, LocalContextType, a_value::AValue, form_compilation::{compile_form, compile_form_attr, compile_norm_form}, name_mangling::{concat_overload_name, rename_fn_alias, NameConcatStyle}, type_anno::ConcreteTypeAnno};
use bacommon::{error::*, vmbuilder::builder_position_at_before};

////////////////////////////////////////////////////////////////////////////////
//// Fn `Form`

#[derive(Debug, Clone)]
pub struct AFn {
    pub name: String,
    pub params: Vec<ConcreteParam>,
    pub ret: Option<ConcreteTypeAnno>,
    pub body: ListData,
}

#[derive(Debug, Clone)]
pub struct ConcreteParam {
    pub formal: String,
    pub type_anno: ConcreteTypeAnno,
}

impl<'ctx> AFn {
    pub(crate) fn compile_declare(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<FunctionType<'ctx>, Box<dyn Error>> {
        if let Some(fn_t) = ctx.fn_map.get(&self.vm_name()) {
            return Ok(fn_t.clone());
        }

        let mut param_types = Vec::with_capacity(self.params.len());

        for param in self.params.iter() {
            param_types.push(param.type_anno.compile(ctx)?.into())
        }

        let fn_t = match &self.ret {
            Some(type_anno) => {
                let ret_t = type_anno.compile(ctx)?;

                ret_t.fn_type(&param_types[..], false)
            }
            None => {
                let ret_t = ctx.vmctx.void_type();

                ret_t.fn_type(&param_types[..], false)
            }
        };

        let _fn_val =
            ctx.vmmod.add_function(&self.vm_name(), fn_t.clone(), None);

        ctx.fn_map.insert(self.vm_name(), fn_t.clone());

        Ok(fn_t)
    }

    pub(crate) fn compile_definition(
        &self,
        ctx: &'ctx mut CompileContext<'ctx>,
        _marker: &'ctx PhantomData<&'ctx ()>
    ) -> Result<(), Box<dyn Error>> {
        let fn_val = ctx.vmmod.get_function(&self.vm_name()).unwrap();

        /* Codegen Function Body */
        let fn_blk = ctx.vmctx.append_basic_block(fn_val, "");

        let binds = self
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                (
                    param.formal.clone(),
                    AValue {
                        type_anno: param.type_anno.clone(),
                        vm_val: fn_val.get_nth_param(i as u32).unwrap(),
                    },
                )
            })
            .collect();

        let lctx =
            ctx.create_lctx(binds, LocalContextType::FnBody(fn_blk), None);

        let builder = lctx.get_builder(&ctx.vmctx);

        match &self.body {
            ListData::Nil(_) => {
                if let Some(ref type_anno) = self.ret {
                    return Err(MissMatchedRetTyError::new_box_err(
                        &type_anno.to_string(),
                    ));
                } else {
                    builder.build_return(None);
                }
            }
            ListData::NonNil(nonnillist) => {
                compile_form(ctx, _marker, lctx, nonnillist.clone())?;
            }
        };

        Ok(())
    }
}

impl AFn {
    pub fn vm_name(&self) -> String {
        let concrete_types = self
            .params
            .iter()
            .map(|param| param.type_anno.clone())
            .collect_vec();

        concat_overload_name(
            &self.name,
            &concrete_types[..],
            NameConcatStyle::Fn,
        )
    }
}

