//! IR Data (Non Recursive)
#![allow(unused_imports)]

use std::convert::{TryFrom, TryInto};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::error::Error;

use bacommon::lexer::SrcLoc;
use indexmap::{IndexMap, indexmap};
use inkwell::values::BasicValueEnum;
use itertools::Itertools;

use crate::error::TrapCode;
use crate::frontend::lexer::Token;
use crate::frontend::manual_parser::BOP_PREC_MAP;

mod syntax;
mod foreign;
mod primitive;

pub use syntax::*;
pub use foreign::*;
pub use primitive::*;


////////////////////////////////////////////////////////////////////////////////
//// Common Trait

pub trait ToBaType {
    fn to_batype(&self) -> BaType;
}

pub trait GetBaType {
    fn get_batype(&self) -> Option<BaType>;
}

pub trait GetLoc {
    fn get_loc(&self) -> SrcLoc;
}

pub trait LLVMIRTag {
    fn tag_str(&self) -> String;
}



////////////////////////////////////////////////////////////////////////////////
//// BaType

#[derive(Debug, Clone)]
pub enum BaType {
    USize,
    I64,
    Int,    // I32
    U8,
    Float,
    VoidUnit,   // Rust Unit,
    RawStr,
    Arr(Box<Self>),

    Range(Box<Self>),
    RangeFrom(Box<Self>),
    RangeTo(Box<Self>),
    RangeFull(),

    Customized(Rc<BaId>),
    ExRefFunProto(CFun)
}

impl fmt::Display for BaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match &self {
            Self::Customized(tyid) => tyid.name.clone(),
            Self::ExRefFunProto(exref_proto) => format!("{:?}", exref_proto),
            _ => format!("{:?}", self)
        })
    }
}

impl PartialEq for BaType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Customized(l0), Self::Customized(r0)) => l0.ty == r0.ty,
            (Self::ExRefFunProto(l0), Self::ExRefFunProto(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for BaType {}

impl std::hash::Hash for BaType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Customized(baid) => {
                baid.name.hash(state)
            },
            Self::ExRefFunProto(exref_funproto) => {
                format!("{:?}", exref_funproto).hash(state)
            },
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// SymItem

#[derive(Debug, Clone)]
pub struct SymItem {
    pub(crate) ty: BaType,
}

