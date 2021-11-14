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

impl<'ctx> ConcreteTypeAnno {
    pub fn compile(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        Ok(match &self {
            Self::Primitive(ref name, addr_mode) => match name.as_str() {
                "usize" | "i64" | "u64" => {
                    let val_t = ctx.vmctx.i64_type();

                    if let AddrMode::Ptr = addr_mode {
                        val_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        val_t.into()
                    }
                }
                "i32" | "u32" => {
                    let val_t = ctx.vmctx.i32_type();

                    if let AddrMode::Ptr = addr_mode {
                        val_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        val_t.into()
                    }
                }
                "i8" | "u8" => {
                    let val_t = ctx.vmctx.i8_type();

                    if let AddrMode::Ptr = addr_mode {
                        val_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        val_t.into()
                    }
                }
                "float" | "f64" => {
                    let val_t = ctx.vmctx.f64_type();

                    if let AddrMode::Ptr = addr_mode {
                        val_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        val_t.into()
                    }
                }
                _ => unreachable!(),
            },
            Self::Struct(name, addr_mode) => {
                if let Some(vm_t) = ctx.struct_map.get(name) {
                    if let AddrMode::Ptr = addr_mode {
                        vm_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        vm_t.clone().into()
                    }
                } else {
                    // collect all form-struct before compile
                    if let Some(form_struct) =
                        ctx.form_struct_map.get(name.as_str()).cloned()
                    {
                        // circular dependency check before that
                        let struct_t =
                            form_struct.compile(ctx)?.into_struct_type();

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

    pub(crate) fn try_into_form_struct(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<AStruct, Box<dyn Error>> {
        if let Self::Struct(name, addr_mode) = self {
            if let AddrMode::Value = addr_mode {
                if let Some(astruct) = ctx.form_struct_map.get(name) {
                    return Ok(astruct.clone());
                }
            }
        }

        Err(XXXError::new_box_err(self.to_string().as_str()))
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
            "u64" | "i64" | "u32" | "i32" | "u8" | "float" | "f64" => {
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



pub(crate) fn compile_concrete_types<'ctx>(
    ctx: &mut CompileContext<'ctx>,
    types_anno: &[ConcreteTypeAnno],
) -> Result<Vec<BasicTypeEnum<'ctx>>, Box<dyn Error>> {
    let mut vm_types = Vec::with_capacity(types_anno.len());

    for type_anno in types_anno.iter() {
        vm_types.push(type_anno.compile(ctx)?)
    }

    Ok(vm_types)
}
