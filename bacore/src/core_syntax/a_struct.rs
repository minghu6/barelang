use std::{collections::HashSet, error::Error};

use inkwell::types::BasicTypeEnum;
use itertools::Itertools;

use super::{
    a_fn::ConcreteParam,
    type_anno::{AddrMode, ConcreteTypeAnnoGetter},
    CompileContext, ConcreteTypeAnno,
};
use crate::error::*;

////////////////////////////////////////////////////////////////////////////////
//// Struct `Form`

#[derive(Debug, Clone)]
pub struct AStruct {
    pub name: String,
    pub fields: Vec<ConcreteField>,
}

pub type ConcreteField = ConcreteParam;

impl<'ctx> AStruct {
    pub fn compile(
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
        ConcreteTypeAnno::Struct(self.name.to_owned(), AddrMode::Value)
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
                super::ConcreteTypeAnno::Primitive(_, _) => (),
                super::ConcreteTypeAnno::Struct(
                    struct_field_type_name,
                    addr_mode,
                ) => {
                    if let AddrMode::Ptr = addr_mode {
                        continue;
                    }

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

    pub(crate) fn index_of_field(&self, field_name: &str) -> Option<usize> {
        if let Some((idx, _)) = self
            .fields
            .iter()
            .find_position(|field| field.formal == field_name)
        {
            Some(idx)
        } else {
            None
        }
    }
}
