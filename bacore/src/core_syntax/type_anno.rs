use std::error::Error;

use inkwell::{types::BasicTypeEnum, AddressSpace};
use itertools::Itertools;

use super::{a_struct::AStruct, CompileContext};
use bacommon::error::*;

////////////////////////////////////////////////////////////////////////////////
//// Common Traits

pub trait ConcreteTypeAnnoGetter {
    fn get_concrete_type_anno(&self) -> ConcreteTypeAnno;
}

////////////////////////////////////////////////////////////////////////////////
//// AddrMode

#[derive(Debug, Clone)]
pub enum AddrMode {
    Value,
    Ptr, // ptr-1
}

impl AddrMode {
    pub(crate) fn poss_postfix(&self) -> String {
        match &self {
            Self::Value => "".to_owned(),
            Self::Ptr => "_ptr".to_owned(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Template Type Annotation

#[derive(Debug, Clone)]
pub enum TemplateTypeAnno {
    TemplateStruct(String, Vec<usize>, AddrMode),

    /// Generic Type Index
    Itself(usize, AddrMode),
}

#[allow(unused)]
impl TemplateTypeAnno {
    pub fn replace_addr_mode(self, new_addr_mode: AddrMode) -> Self {
        match self {
            Self::TemplateStruct(name, idxs, _addr_mode) => {
                Self::TemplateStruct(name, idxs, new_addr_mode)
            }
            Self::Itself(idx, _addr_mode) => Self::Itself(idx, new_addr_mode),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Concrete Type Annotation

#[derive(Debug, Clone)]
pub enum ConcreteTypeAnno {
    Primitive(String, AddrMode),
    Struct(String, AddrMode),
}

#[allow(unused)]
impl ConcreteTypeAnno {
    pub fn replace_addr_mode(self, new_addr_mode: AddrMode) -> Self {
        match self {
            Self::Primitive(name, _addr_mode) => {
                Self::Primitive(name, new_addr_mode)
            }
            Self::Struct(name, _addr_mode) => Self::Struct(name, new_addr_mode),
        }
    }
}


impl ToString for ConcreteTypeAnno {
    fn to_string(&self) -> String {
        match &self {
            ConcreteTypeAnno::Primitive(name, addr_mode) => {
                name.to_owned() + &addr_mode.poss_postfix()
            }
            ConcreteTypeAnno::Struct(name, addr_mode) => {
                name.to_owned() + &addr_mode.poss_postfix()
            }
        }
    }
}

/// *usize => usize.ptr_t()
///
impl From<&str> for ConcreteTypeAnno {
    fn from(input: &str) -> Self {
        let (addr_mode, input_val) = if input.starts_with("*") {
            (AddrMode::Ptr, input.strip_prefix("*").unwrap())
        } else {
            (AddrMode::Value, input)
        };

        match input_val {
            "u64" | "i64" | "usize" |"u32" | "i32" | "u8" | "float" | "f64" => {
                ConcreteTypeAnno::Primitive(input_val.to_owned(), addr_mode)
            }
            _ => ConcreteTypeAnno::Struct(input_val.to_owned(), addr_mode),
        }
    }
}

impl ConcreteTypeAnno {}

////////////////////////////////////////////////////////////////////////////////
//// TypeAnno

#[derive(Debug, Clone)]
pub(crate) enum TypeAnno {
    Template(TemplateTypeAnno),
    Concrete(ConcreteTypeAnno),
}

impl<'ctx> TypeAnno {
    pub(crate) fn finalize(
        &self,
        ctx: &mut CompileContext<'ctx>,
        concrete_types: &[ConcreteTypeAnno],
    ) -> Result<ConcreteTypeAnno, Box<dyn Error>> {
        Ok(match self {
            TypeAnno::Template(anno) => match anno {
                TemplateTypeAnno::TemplateStruct(name, idxs, addr_mode) => {
                    let construction =
                        ctx.template_struct_map.get(name).unwrap().clone();

                    let construction_types = idxs
                        .into_iter()
                        .cloned()
                        .map(|idx| concrete_types[idx].clone())
                        .collect();

                    let expanded_struct =
                        construction.expand(ctx, construction_types)?;

                    expanded_struct
                        .get_concrete_type_anno()
                        .replace_addr_mode(addr_mode.clone())
                }
                TemplateTypeAnno::Itself(idx, addr_mode) => concrete_types
                    [idx.clone()]
                .clone()
                .replace_addr_mode(addr_mode.clone()),
            },
            TypeAnno::Concrete(concrete_type) => concrete_type.clone(),
        })
    }
}


