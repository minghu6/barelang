use std::error::Error;

use convert_case::{Case, Casing};
use inkwell::types::BasicTypeEnum;
use itertools::Itertools;

use crate::core_syntax::form_struct::ConcreteField;

use super::{
    form_struct::AStruct, CompileContext, ConcreteTypeAnno,
    ConcreteTypeAnnoGetter, Param, TemplateTypeAnno, TypeAnno,
};

////////////////////////////////////////////////////////////////////////////////
//// `TemplateStruct` Form

#[derive(Debug, Clone)]
pub struct TemplateStruct {
    pub name: String,
    pub generic_placeholder_num: usize,
    pub fields: Vec<Field>,
}

pub type Field = Param;

impl<'ctx> TemplateStruct {
    fn expand(
        &self,
        ctx: &mut CompileContext<'ctx>,
        concrete_types: Vec<ConcreteTypeAnno>,
    ) -> Result<AStruct, Box<dyn Error>> {
        let postfix = concrete_types
            .iter()
            .map(|anno| anno.to_string().to_case(Case::Camel))
            .join("");

        let concrete_name = self.name.to_owned() + &postfix;

        // load from memory cache
        if let Some(struct_t) = ctx.form_struct_map.get(&concrete_name) {
            return Ok(struct_t.clone());
        }

        // expand
        let mut concrete_fields = Vec::with_capacity(self.fields.len());
        for field in self.fields.iter() {
            let type_anno = match &field.type_anno {
                TypeAnno::Template(anno) => match anno {
                    TemplateTypeAnno::TemplateStruct(name, idxs) => {
                        let field_construction =
                            ctx.template_struct_map.get(name).unwrap().clone();

                        let field_construction_types = idxs
                            .into_iter()
                            .cloned()
                            .map(|idx| concrete_types[idx].clone())
                            .collect();

                        let field_expanded_struct = field_construction
                            .expand(ctx, field_construction_types)?;

                        field_expanded_struct.get_concrete_type_anno()
                    }
                    TemplateTypeAnno::Itself(idx) => {
                        concrete_types[idx.clone()].clone()
                    }
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
