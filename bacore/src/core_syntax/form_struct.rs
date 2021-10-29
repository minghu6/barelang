use std::{collections::HashSet, error::Error};

use inkwell::types::BasicTypeEnum;

use super::{
    Compile, CompileContext, ConcreteParam, ConcreteTypeAnno,
    ConcreteTypeAnnoGetter, TypeAnno,
};
use crate::error::*;

////////////////////////////////////////////////////////////////////////////////
//// Struct Form

#[derive(Debug, Clone)]
pub struct AStruct {
    pub name: String,
    pub fields: Vec<ConcreteField>,
}

pub type ConcreteField = ConcreteParam;

impl<'ctx> Compile<'ctx> for AStruct {
    fn compile(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        if let Some(astruct_t) = ctx.struct_map.get(&self.name) {
            return Ok(astruct_t.clone().into());
        }

        let mut field_types = vec![];

        for field in self.fields.iter() {
            field_types.push(field.type_anno.compile(ctx)?)
        }

        let struct_t = ctx.vmctx.struct_type(&field_types[..], false);

        ctx.struct_map
            .insert(self.name.to_owned(), struct_t.clone());

        Ok(struct_t.into())
    }
}

impl ConcreteTypeAnnoGetter for AStruct {
    fn get_concrete_type_anno(&self) -> ConcreteTypeAnno {
        ConcreteTypeAnno::Struct(self.name.to_owned())
    }
}

impl AStruct {
    pub(crate) fn do_circular_dependency_check(
        &self,
        ctx: &CompileContext,
        queried_set: &mut HashSet<String>,
    ) -> Result<(), Box<dyn Error>> {
        queried_set.insert(self.name.to_owned());

        for field in self.fields.iter() {
            match &field.type_anno {
                super::ConcreteTypeAnno::Primitive(_) => (),
                super::ConcreteTypeAnno::Struct(struct_field_type_name) => {
                    if queried_set.contains(struct_field_type_name) {
                        return Err(CircularDependencyError::new_box_err(
                            struct_field_type_name,
                        ));
                    }

                    let form_struct = ctx
                        .form_struct_map
                        .get(struct_field_type_name)
                        .unwrap();

                    form_struct
                        .do_circular_dependency_check(ctx, queried_set)?;
                }
            }
        }

        Ok(())
    }
}
