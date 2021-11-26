use std::error::Error;

use inkwell::{
    types::{BasicType, BasicTypeEnum, IntType},
    AddressSpace,
};
use itertools::Itertools;
use lisparser::data::{BraceMapData, LitData};
use lisparser::*;

use super::{a_struct::AStruct, a_value::AValue, r#const::*, CompileContext};
use bacommon::{config::usize_len, error::*};

#[inline]
pub fn is_int(name: &str) -> bool {
    match name {
        BOOL | U8 | U16 | U32 | U64 | U128 | USIZE | ISIZE | I8 | I16
        | I32 | I64 | I128 => true,
        _ => false,
    }
}

#[allow(unused)]
#[inline]
pub fn is_unsigned(name: &str) -> bool {
    match name {
        BOOL | U8 | U16 | U32 | U64 | U128 | USIZE => true,
        _ => false,
    }
}

#[inline]
pub fn is_float(name: &str) -> bool {
    match name {
        F64 | FLOAT => true,
        _ => false,
    }
}

#[inline]
pub fn is_primary(name: &str) -> bool {
    is_int(name) || is_float(name)
}

#[inline]
pub fn primary_size(name: &str) -> usize {
    match name {
        U8 | I8 => 1,
        U16 | I16 => 2,
        U32 | I32 => 4,
        USIZE | ISIZE => usize_len(),
        U64 | I64 => 8,
        U128 | I128 => 16,
        F64 | FLOAT => 8,
        _ => unreachable!(name),
    }
}

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
    pub fn poss_postfix(&self) -> String {
        match &self {
            Self::Value => "".to_owned(),
            Self::Ptr => "_ptr".to_owned(),
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            AddrMode::Value => false,
            AddrMode::Ptr => true,
        }
    }

    pub fn is_val(&self) -> bool {
        match self {
            AddrMode::Value => true,
            AddrMode::Ptr => false,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            AddrMode::Value => ADDR_VAL,
            AddrMode::Ptr => ADDR_PTR,
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
            Self::Struct(name, _addr_mode) => {
                Self::Struct(name, new_addr_mode)
            }
        }
    }

    pub fn get_val_type(&self) -> Self {

        self.clone().replace_addr_mode(AddrMode::Value)

    }

    pub fn addr_mode(&self) -> &AddrMode {
        match self {
            Self::Primitive(_, addr_mode) => addr_mode,
            Self::Struct(_, addr_mode) => addr_mode,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            ConcreteTypeAnno::Primitive(name, _) => name,
            ConcreteTypeAnno::Struct(name, _) => name,
        }
    }

    pub fn is_ptr(&self) -> bool {
        self.addr_mode().is_ptr()
    }

    pub fn is_val(&self) -> bool {
        self.addr_mode().is_val()
    }

    pub fn is_int_val(&self) -> bool {
        match self {
            Self::Primitive(primary_str, addr_mode) => {
                is_int(primary_str) && addr_mode.is_val()
            }
            _ => false,
        }
    }

    pub fn is_float_val(&self) -> bool {
        match self {
            Self::Primitive(primary_str, addr_mode) => {
                is_float(primary_str) && addr_mode.is_val()
            }
            _ => false,
        }
    }

    pub fn is_num_val(&self) -> bool {
        self.is_int_val() && self.is_float_val()
    }

    pub fn is_unsigned(&self) -> bool {
        is_unsigned(self.type_name())
    }

    pub fn unparse(&self) -> BraceMapData {
        match self {
            ConcreteTypeAnno::Primitive(type_name, addr) => {
                brace_map!(
                    key!(KEY_TYPE_PRIMITIVE) => sym!(self.type_name()),
                    key!(KEY_ADDR) => sym!(addr.as_str())
                )
            }
            ConcreteTypeAnno::Struct(type_name, addr) => {
                brace_map!(
                    key!(KEY_TYPE_STRUCT) => sym!(self.type_name()),
                    key!(KEY_ADDR) => sym!(addr.as_str())
                )
            }
        }
    }

    // pub fn decompile_vmtype_int(vm_int_t: IntType) -> Self {
    //     match (vm_int_t.get_bit_width(), vm_int_t.) {

    //     }

    // }

    pub fn num_len(&self) -> Option<usize> {
        match self {
            ConcreteTypeAnno::Primitive(type_name, addr_mode) => {
                if addr_mode.is_ptr() {
                    return None;
                }

                Some(primary_size(type_name))
            }
            ConcreteTypeAnno::Struct(_, _) => None,
        }
    }

    // /// ta < tb
    // pub fn try_compare(ta: Self, tb: Self) -> Result<bool, Box<dyn Error>> {

    //     if ta.is_num_val() && tb.is_num_val() {

    //     }
    //     else {
    //         Err(NonPrimitiveCompareError::new_box_err(&format!(
    //             "ta: {:?}, tb: {:?}", ta, tb
    //         )))
    //     }

    // }

    pub fn i8() -> Self {
        ConcreteTypeAnno::Primitive(I8.to_owned(), AddrMode::Value)
    }

    pub fn u8() -> Self {
        ConcreteTypeAnno::Primitive(U8.to_owned(), AddrMode::Value)
    }

    pub fn u16() -> Self {
        ConcreteTypeAnno::Primitive(U16.to_owned(), AddrMode::Value)
    }

    pub fn i16() -> Self {
        ConcreteTypeAnno::Primitive(I16.to_owned(), AddrMode::Value)
    }

    pub fn u32() -> Self {
        ConcreteTypeAnno::Primitive(U32.to_owned(), AddrMode::Value)
    }

    pub fn i32() -> Self {
        ConcreteTypeAnno::Primitive(I32.to_owned(), AddrMode::Value)
    }

    pub fn u64() -> Self {
        ConcreteTypeAnno::Primitive(U64.to_owned(), AddrMode::Value)
    }

    pub fn i64() -> Self {
        ConcreteTypeAnno::Primitive(I64.to_owned(), AddrMode::Value)
    }

    pub fn u128() -> Self {
        ConcreteTypeAnno::Primitive(U128.to_owned(), AddrMode::Value)
    }

    pub fn i128() -> Self {
        ConcreteTypeAnno::Primitive(I128.to_owned(), AddrMode::Value)
    }

    pub fn bool() -> Self {
        ConcreteTypeAnno::Primitive(BOOL.to_owned(), AddrMode::Value)
    }

    pub fn f64() -> Self {
        ConcreteTypeAnno::Primitive(F64.to_owned(), AddrMode::Value)
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
impl From<&str> for ConcreteTypeAnno {
    fn from(input: &str) -> Self {
        let (addr_mode, input_val) = if input.starts_with("*") {
            (AddrMode::Ptr, input.strip_prefix("*").unwrap())
        } else {
            (AddrMode::Value, input)
        };

        if is_primary(input_val) {
            ConcreteTypeAnno::Primitive(input_val.to_owned(), addr_mode)
        } else {
            ConcreteTypeAnno::Struct(input_val.to_owned(), addr_mode)
        }
    }
}

// impl From<LitData> for ConcreteTypeAnno {

// }

// pub fn should_be_int<'ctx>(
//     name: &str,
//     args_vec: &[AValue<'ctx>],
//     idx: usize,
// ) -> Result<AValue<'ctx>, Box<dyn Error>> {
//     let elem = args_vec[idx].clone();

//     if elem.type_anno.is_int_val() {
//         Ok(elem)
//     } else {
//         Err(MalformedSyntaxError::new_box_err(&format!(
//             "{} clause-{} should be int, found {:?}",
//             name, idx, elem.type_anno
//         )))
//     }
// }

pub fn should_be_ptr<'ctx>(
    name: &str,
    args_vec: &[AValue<'ctx>],
    idx: usize,
) -> Result<AValue<'ctx>, Box<dyn Error>> {
    let elem = args_vec[idx].clone();

    if elem.type_anno.is_ptr() {
        Ok(elem)
    } else {
        Err(MalformedSyntaxError::new_box_err(&format!(
            "{} clause-{} should be int, found {:?}",
            name, idx, elem.type_anno
        )))
    }
}

pub fn should_be_unsigned_int<'ctx>(
    name: &str,
    args_vec: &[AValue<'ctx>],
    idx: usize,
) -> Result<AValue<'ctx>, Box<dyn Error>> {
    let elem = args_vec[idx].clone();

    if elem.type_anno.is_unsigned() {
        Ok(elem)
    } else {
        Err(MalformedSyntaxError::new_box_err(&format!(
            "{} clause-{} should be unsigned int, found {:?}",
            name, idx, elem.type_anno
        )))
    }
}


pub fn should_be_3clause<'ctx>(
    name: &str,
    args_vec: &[AValue<'ctx>],
) -> Result<(), Box<dyn Error>> {
    if args_vec.len() != 3 {
        Err(MalformedSyntaxError::new_box_err(&format!(
            "{} expect 3-clause, however, found {}",
            name,
            args_vec.len()
        )))
    } else {
        Ok(())
    }
}

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
                        .collect_vec();

                    let expanded_struct =
                        construction.expand(ctx, &construction_types[..])?;

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
