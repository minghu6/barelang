//! Codegen Data

use std::{
    cell::RefCell,
    convert::{TryFrom, TryInto},
    rc::Rc,
};

use indexmap::{indexmap, IndexMap};
use inkwell::values::{
    BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
};

use crate::{
    error::TrapCode,
    middleware::datair::{BaFunHdr, BaType},
    utils::usize_len,
};

pub trait TryGetBasicValue<'ctx> {
    fn try_get_basic_value(
        &self,
    ) -> Result<BasicValueEnum<'ctx>, Box<dyn std::error::Error>>;
}

///////////////////////////////////////////////////////////////////////////
//// CodeGen Scope

pub type CGSymTbl<'ctx> = IndexMap<String, CGValue<'ctx>>;

#[derive(Debug, Clone)]
pub struct CGScope<'ctx> {
    pub(crate) parent: Option<Rc<RefCell<Self>>>,
    pub(crate) symtbl: CGSymTbl<'ctx>,
}

impl<'ctx> CGScope<'ctx> {
    pub fn new() -> Self {
        Self {
            parent: None,
            symtbl: indexmap! {},
        }
    }

    pub fn get_sym_item(&self, key: &str) -> Option<CGValue<'ctx>> {
        if let Some(item) = self.symtbl.get(key) {
            Some(item.clone())
        } else if let Some(paren) = &self.parent {
            paren.as_ref().borrow().get_sym_item(key)
        } else {
            None
        }
    }
}

///////////////////////////////////////////////////////////////////////////
//// CodeGen Void
pub struct CGVoid;

///////////////////////////////////////////////////////////////////////////
//// CodeGen Fn

pub struct CGFn<'ctx> {
    pub hdr: BaFunHdr,
    pub val: FunctionValue<'ctx>,
}

impl<'ctx> From<(BaFunHdr, FunctionValue<'ctx>)> for CGFn<'ctx> {
    fn from(income: (BaFunHdr, FunctionValue<'ctx>)) -> Self {
        let (hdr, fv) = income;

        CGFn { hdr, val: fv }
    }
}

///////////////////////////////////////////////////////////////////////////
//// CodeGen Value

#[derive(Debug, Clone)]
pub enum CGValue<'ctx> {
    Pure(CGPureValue<'ctx>),
    Ref(CGRef<'ctx>),
}


impl<'ctx> TryGetBasicValue<'ctx> for CGValue<'ctx> {
    fn try_get_basic_value(
        &self,
    ) -> Result<BasicValueEnum<'ctx>, Box<(dyn std::error::Error + 'static)>>
    {
        Ok(match &self {
            CGValue::Pure(ref cgpure) => match &cgpure {
                CGPureValue::Int(cgint) => cgint.val.into(),
                CGPureValue::Float(cgfloat) => cgfloat.val.into(),
                CGPureValue::RawStr(cgstr) => cgstr.val.into(),
                CGPureValue::VoidUnit => {
                    return Err(TrapCode::ToCGValOnVoid.emit_box_err())
                }
            },
            CGValue::Ref(cgref) => match cgref {
                CGRef::Arr(arr_rc) => arr_rc.as_ref().borrow().start.into(),
            },
        })
    }
}


impl<'ctx> TryFrom<(BaType, BasicValueEnum<'ctx>)> for CGValue<'ctx> {
    type Error = Box<dyn std::error::Error>;

    fn try_from(
        income: (BaType, BasicValueEnum<'ctx>),
    ) -> Result<Self, Self::Error> {
        let (ty, bv) = income;

        let cgval = match &ty {
            BaType::VoidUnit => {
                return Err(TrapCode::ToCGValOnVoid.emit_box_err())
            }
            BaType::Int => CGValue::Pure(CGPureValue::Int(CGInt {
                length: 4,
                signed: true,
                val: bv.into_int_value(),
            })),
            BaType::U8 => CGValue::Pure(CGPureValue::Int(CGInt {
                length: 1,
                signed: false,
                val: bv.into_int_value(),
            })),
            BaType::USize => CGValue::Pure(CGPureValue::Int(CGInt {
                length: usize_len(),
                signed: false,
                val: bv.into_int_value(),
            })),
            BaType::I64 => CGValue::Pure(CGPureValue::Int(CGInt {
                length: 64,
                signed: false,
                val: bv.into_int_value(),
            })),
            BaType::Float => CGValue::Pure(CGPureValue::Float(CGFloat {
                length: 64,
                val: bv.into_float_value(),
            })),
            BaType::RawStr => CGValue::Pure(CGPureValue::RawStr(CGRawStr {
                val: bv.into_pointer_value(),
            })),
            BaType::Arr(_) => {
                return Err(TrapCode::RefTypeTryIntoPureValue.emit_box_err())
            }
            BaType::Customized(_id_rc) => todo!(),
            BaType::ExRefFunProto(ref _exref_funproto) => {
                todo!()
            }
        };

        Ok(cgval)
    }
}

impl<'ctx> TryInto<CGPureValue<'ctx>> for CGValue<'ctx> {
    type Error = Box<dyn std::error::Error>;

    fn try_into(self) -> Result<CGPureValue<'ctx>, Self::Error> {
        if let CGValue::Pure(pure_val) = self {
            Ok(pure_val)
        } else {
            unreachable!()
        }
    }
}

impl<'ctx> TryInto<CGArr<'ctx>> for CGValue<'ctx> {
    type Error = Box<dyn std::error::Error>;

    fn try_into(self) -> Result<CGArr<'ctx>, Box<dyn std::error::Error>> {
        match self {
            CGValue::Pure(_) => unreachable!(),
            CGValue::Ref(cgref) => {
                match cgref {
                    CGRef::Arr(cgarr) => {
                        Ok(cgarr.as_ref().borrow().clone())
                    },
                }
            },
        }
    }
}




///////////////////////////////////////////////////////////////////////////
//// CodeGen Pure Value

#[derive(Debug, Clone)]
pub enum CGPureValue<'ctx> {
    Int(CGInt<'ctx>),
    Float(CGFloat<'ctx>),
    RawStr(CGRawStr<'ctx>),
    VoidUnit,
}

impl<'ctx> CGPureValue<'ctx> {
    pub fn fork_with(&self, bv: BasicValueEnum<'ctx>) -> Self {
        match &self {
            CGPureValue::Float(float) => {
                let mut dup = float.clone();
                dup.val = bv.into_float_value();
                CGPureValue::Float(dup)
            }
            CGPureValue::Int(int) => {
                let mut dup = int.clone();
                dup.val = bv.into_int_value();
                CGPureValue::Int(dup)
            }
            CGPureValue::RawStr(rawstr) => {
                let mut dup = rawstr.clone();
                dup.val = bv.into_pointer_value();
                CGPureValue::RawStr(dup)
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CGInt<'ctx> {
    pub length: usize, // bytes
    pub signed: bool,
    pub val: IntValue<'ctx>,
}

#[derive(Debug, Clone)]
pub struct CGFloat<'ctx> {
    pub length: usize,
    pub val: FloatValue<'ctx>,
}

#[derive(Debug, Clone)]
pub struct CGRawStr<'ctx> {
    pub val: PointerValue<'ctx>,
}

///////////////////////////////////////////////////////////////////////////
//// CodeGen Ref

#[derive(Debug, Clone)]
pub enum CGRef<'ctx> {
    Arr(Rc<RefCell<CGArr<'ctx>>>),
}

#[derive(Debug, Clone)]
pub struct CGArr<'ctx> {
    pub ty: BaType,
    pub len: usize,
    pub capacity: usize, // malloc mem: capacity * unit_type.len() bytes
    pub start: PointerValue<'ctx>,
}

// ///////////////////////////////////////////////////////////////////////////
// //// CodeGen Primitive Type

// pub struct CGVar<'ctx> {
//     pub ptr:
// }

#[derive(Debug, Clone)]
pub struct MemLayout {
    pub contiguous_offset: Vec<usize>, // 0, 4, 8, 12 for array of i32
    pub symtbl: IndexMap<String, usize>, // value is idx of contiguous offset vector
}

pub trait GetMemLayout {
    fn get_layout(&self) -> MemLayout;
}

// impl GetMemLayout for BaType {
//     fn get_layout(&self) -> MemLayout {
//         match &self {
//             BaType::USize => MemLayout {
//                 contiguous_offset: vec![0, ]
//             },
//             BaType::I64 => todo!(),
//             BaType::Int => todo!(),
//             BaType::U8 => todo!(),
//             BaType::Float => todo!(),
//             BaType::VoidUnit => todo!(),
//             BaType::RawStr => todo!(),
//             BaType::Customized(_) => todo!(),
//             BaType::ExRefFunProto(_) => todo!(),
//         }
//     }
// }
