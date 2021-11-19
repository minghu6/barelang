use std::error::Error;

use bacommon::error::UnmatchedGenericNumberError;
use convert_case::{Case, Casing};
use inkwell::types::BasicTypeEnum;
use itertools::Itertools;

use crate::core_syntax::a_struct::ConcreteField;

use super::{
    a_struct::AStruct,
    name_mangling::{concat_overload_name, NameConcatStyle},
    template_fn::Param,
    type_anno::{
        ConcreteTypeAnno, ConcreteTypeAnnoGetter, TemplateTypeAnno, TypeAnno,
    },
    CompileContext,
};

////////////////////////////////////////////////////////////////////////////////
//// `TemplateStruct` Form

#[derive(Debug, Clone)]
pub(crate) struct TemplateStruct {
    pub(crate) name: String,
    pub(crate) generic_placeholder_num: usize,
    pub(crate) fields: Vec<Field>,
}

pub(crate) type Field = Param;

impl<'ctx> TemplateStruct {
    pub(crate) fn expand(
        &self,
        ctx: &mut CompileContext<'ctx>,
        concrete_types: Vec<ConcreteTypeAnno>,
    ) -> Result<AStruct, Box<dyn Error>> {
        if concrete_types.len() != self.generic_placeholder_num {
            return Err(UnmatchedGenericNumberError::new_box_err(
                format!(
                    "expect: {}, found: {}",
                    concrete_types.len(),
                    self.generic_placeholder_num
                )
                .as_str(),
            ));
        }

        let concrete_name = concat_overload_name(
            &self.name,
            &concrete_types,
            NameConcatStyle::Struct,
        );

        // load from memory cache
        if let Some(astruct) = ctx.form_struct_map.get(&concrete_name) {
            return Ok(astruct.clone());
        }

        // expand
        let mut concrete_fields = Vec::with_capacity(self.fields.len());
        for field in self.fields.iter() {
            let type_anno = match &field.type_anno {
                TypeAnno::Template(anno) => match anno {
                    TemplateTypeAnno::TemplateStruct(
                        name,
                        idxs,
                        addr_mode,
                    ) => {
                        let field_construction =
                            ctx.template_struct_map.get(name).unwrap().clone();

                        let field_construction_types = idxs
                            .into_iter()
                            .cloned()
                            .map(|idx| concrete_types[idx].clone())
                            .collect();

                        let field_expanded_struct = field_construction
                            .expand(ctx, field_construction_types)?;

                        field_expanded_struct
                            .get_concrete_type_anno()
                            .replace_addr_mode(addr_mode.clone())
                    }
                    TemplateTypeAnno::Itself(idx, addr_mode) => concrete_types
                        [idx.clone()]
                    .clone()
                    .replace_addr_mode(addr_mode.clone()),
                },
                TypeAnno::Concrete(concrete_type) => concrete_type.clone(),
            };

            let formal = field.formal.to_owned();

            concrete_fields.push(ConcreteField { formal, type_anno })
        }

        let astruct = AStruct {
            name: concrete_name.clone(),
            fields: concrete_fields,
        };

        ctx.form_struct_map.insert(concrete_name, astruct.clone());

        Ok(astruct)
    }
}
