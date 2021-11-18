use std::error::Error;

use convert_case::{Case, Casing};
use inkwell::types::BasicTypeEnum;
use itertools::Itertools;
use lisparser::data::ListData;

use crate::core_syntax::a_struct::ConcreteField;

use super::{
    a_fn::AFn,
    name_mangling::{concat_overload_name, NameConcatStyle},
    type_anno::TypeAnno,
    CompileContext, ConcreteTypeAnno,
};

////////////////////////////////////////////////////////////////////////////////
//// `TemplateFn` Form

#[allow(unused)]
#[derive(Debug, Clone)]
pub(crate) struct TemplateFn {
    pub(crate) name: String,
    pub(crate) generic_placeholder_num: usize,
    pub(crate) params: Vec<Param>,
    pub(crate) ret: Option<TypeAnno>,
    pub(crate) body: ListData,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub(crate) struct Param {
    pub(crate) formal: String,
    pub(crate) type_anno: TypeAnno,
}

impl<'ctx> TemplateFn {
    pub(crate) fn expand(
        &self,
        ctx: &mut CompileContext<'ctx>,
        concrete_types: &[ConcreteTypeAnno],
    ) -> Result<AFn, Box<dyn Error>> {
        let concrete_name = concat_overload_name(
            &self.name,
            &concrete_types,
            NameConcatStyle::Fn,
        );

        // load from memory cache
        if let Some(afn) = ctx.form_fn_map.get(&concrete_name) {
            return Ok(afn.clone());
        }

        // expand
        let mut concrete_params = Vec::with_capacity(self.params.len());
        for param in self.params.iter() {
            let type_anno = param.type_anno.finalize(ctx, concrete_types)?;
            let formal = param.formal.to_owned();

            concrete_params.push(ConcreteField { formal, type_anno })
        }
        let ret = if let Some(ref ret_type_anno) = self.ret {
            Some(ret_type_anno.finalize(ctx, concrete_types)?)
        } else {
            None
        };

        let afn = AFn {
            name: concrete_name.clone(),
            params: concrete_params,
            body: self.body.clone(),
            ret,
        };

        ctx.form_fn_map.insert(concrete_name, afn.clone());

        Ok(afn)
    }
}
