//! IR Data (Non Recursive)
#![allow(unused_imports)]

use std::convert::{TryFrom, TryInto};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::error::Error;

use indexmap::{IndexMap, indexmap};
use inkwell::values::BasicValueEnum;
use itertools::Itertools;

use crate::error::TrapCode;
use crate::frontend::lexer::{SrcLoc, Token};
use crate::frontend::manual_parser::BOP_PREC_MAP;


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
//// BaFunHdr

#[derive(Debug, Clone)]
pub struct BaFunHdr {
    pub name: BaId,
    pub params: Vec<BaParam>,
    pub ret: BaType
}

impl BaFunHdr {
    pub fn name(&self) -> String {
        self.name.name.to_owned()
    }
}

impl From<(BaId, Vec<BaParam>, BaType)> for BaFunHdr {
    fn from(triple: (BaId, Vec<BaParam>, BaType)) -> Self {
        Self {
            name: triple.0,
            params: triple.1,
            ret: triple.2
        }
    }
}

impl BaFunHdr {
    pub fn as_key(&self) -> BaFunKey {
        BaFunKey(
            self.name.name.clone(),
            self.params
            .iter()
            .map(|param| param.ty.clone())
            .collect_vec()
        )
    }
}

impl GetLoc for BaFunHdr {
    fn get_loc(&self) -> SrcLoc {
        self.name.get_loc()
    }
}


////////////////////////////////////////////////////////////////////////////////
//// BaParam

#[derive(Debug, Clone)]
pub struct BaParam {
    pub formal: String,
    pub ty: BaType,
}

impl From<BaId> for BaParam {
    fn from(id: BaId) -> Self {
        Self {
            formal: id.name,
            ty: id.ty.unwrap()
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// BaFunKey

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BaFunKey(pub String, pub Vec<BaType>);


////////////////////////////////////////////////////////////////////////////////
//// BaLit

#[derive(Debug, Clone)]
pub struct BaUsize {
    pub val: usize,
    pub loc: SrcLoc
}

#[derive(Debug, Clone)]
pub struct BaI64 {
    pub val: i64,
    pub loc: SrcLoc
}

#[derive(Debug, Clone)]
pub struct BaI32 {
    pub val: i32,
    pub loc: SrcLoc
}

#[derive(Debug, Clone)]
pub struct BaU8 {
    pub val: u8,
    pub loc: SrcLoc
}

#[derive(Debug, Clone)]
pub struct BaFloat {
    pub val: f64,
    pub loc: SrcLoc
}

#[derive(Debug, Clone)]
pub struct BaStr {
    pub val: String,
    pub loc: SrcLoc
}

impl LLVMIRTag for BaStr {
    fn tag_str(&self) -> String {
        format!("{}%bastr", &self.loc)
    }
}

///
/// Default Int Type [reference this](https://github.com/rust-lang/rfcs/blob/master/text/0212-restore-int-fallback.md#rationale-for-the-choice-of-defaulting-to-i32)
///
#[derive(Debug, Clone)]
pub enum BaLit {
    USize(BaUsize),
    I64(BaI64),
    I32(BaI32),
    U8(BaU8),
    Float(BaFloat), // 64 bit float
    Str(BaStr),     // Unicode String

}

impl ToBaType for BaLit {
    fn to_batype(&self) -> BaType {
        match &self {
            Self::Float(_) => BaType::Float,
            Self::I32(_) => BaType::Int,
            Self::I64(_) => BaType::I64,
            Self::USize(_) => BaType::USize,
            Self::U8(_) => BaType::U8,
            Self::Str(_) => BaType::RawStr
        }
    }
}

impl GetLoc for BaLit {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Float(f) => f.loc.clone(),
            Self::I32(i32) => i32.loc.clone(),
            Self::I64(i64) => i64.loc.clone(),
            Self::USize(u) => u.loc.clone(),
            Self::U8(u8) => u8.loc.clone(),
            Self::Str(str) => str.loc.clone()
        }
    }
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
    Customized(Rc<BaId>),
    ExRefFunProto(ExRefFunProto)
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
//// BaPriVal

#[derive(Debug, Clone)]
pub enum BaPriVal {
    Lit(BaLit),
    Id(BaId),
    Vector(BaVector)
}

impl TryInto<BaId> for BaPriVal {
    type Error = Box<dyn Error>;

    fn try_into(self) -> Result<BaId, Self::Error> {
        match self {
            Self::Id(id) => Ok(id),
            _ => Err(TrapCode::PriValTryIntoIdFailed(&self).emit_box_err())
        }
    }
}

impl GetBaType for BaPriVal {
    fn get_batype(&self) -> Option<BaType> {
        match &self {
            Self::Id(id) => {
                if let Some(ty) = &id.ty {
                    Some(ty.clone())
                }
                else {
                    None
                }
            },
            Self::Lit(lit) => Some(lit.to_batype()),
            Self::Vector(vec) => vec.get_batype()
        }
    }
}

impl GetLoc for BaPriVal {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Lit(lit) => lit.get_loc(),
            Self::Id(id) => id.get_loc(),
            Self::Vector(vec) => vec.get_loc()
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//// BaVector

#[derive(Debug, Clone)]
pub enum BaVector {
    Array(BaArray)
}

impl GetBaType for BaVector {
    fn get_batype(&self) -> Option<BaType> {
        match &self {
            Self::Array(barr) => {
                barr.get_batype()
            }
        }
    }
}

impl GetLoc for BaVector {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Array(barr) => {
                barr.get_loc()
            }
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//// BaArray

#[derive(Debug, Clone)]
pub struct BaArray {
    pub ty: Option<BaType>,
    pub elems: Vec<BaPriVal>,
    pub srcloc: SrcLoc
}

impl GetBaType for BaArray {
    fn get_batype(&self) -> Option<BaType> {
        if let Some(ty) = &self.ty {
            Some(BaType::Arr(Box::new(ty.clone())))
        }
        else {
            None
        }
    }
}

impl GetLoc for BaArray {
    fn get_loc(&self) -> SrcLoc {
        self.srcloc.clone()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// BaFunCall

#[derive(Debug, Clone)]
pub struct BaFunCall {
    pub name: BaId,
    pub args: Vec<BaPriVal>,
    pub ret: BaType
}

impl BaFunCall {
    // pub fn get_ret_type(&self) -> Option<BaType> {
    //     if let Some(ty) = &self.name.ty {
    //         match ty {
    //             BaType::ExRefFunProto(exref_proto) => {
    //                 Some(exref_proto.ret.as_type())
    //             },
    //             _ => unreachable!()
    //         }
    //     }
    //     else {
    //         None
    //     }
    // }

    pub fn get_fun_key(&self) -> Result<BaFunKey, Box<dyn Error>> {
        let mut collector = vec![];

        for arg in self.args.iter() {
            if let Some(ty) = arg.get_batype() {
                collector.push(ty);
            }
            else {
                match &arg {
                    BaPriVal::Id(id) => {
                        return Err(TrapCode::UnableToInferType(id).emit_box_err())
                    },
                    _ => unreachable!()
                }
            }
        }


        Ok(BaFunKey(
            self.name.name.clone(),
            collector
        ))
    }
}

impl GetLoc for BaFunCall {
    fn get_loc(&self) -> SrcLoc {
        self.name.get_loc()
    }
}


////////////////////////////////////////////////////////////////////////////////
//// BaId

#[derive(Debug, Clone)]
pub struct BaId {
    pub name: String,
    pub splid: Option<BaSplId>,
    pub ty: Option<BaType>,
    pub loc: SrcLoc
}

impl BaId {
    pub fn sym(&self) -> String {
        if let Some(splid) = &self.splid {
            format!("{:?}#{}", splid, self.name)
        }
        else {
            self.name.clone()
        }
    }
}

impl GetLoc for BaId {
    fn get_loc(&self) -> SrcLoc {
        self.loc.clone()
    }
}


#[derive(Debug, Clone)]
pub enum BaSplId {
    RS,
    Arr
}


////////////////////////////////////////////////////////////////////////////////
//// BaParameter

pub type BaParameter = BaId;


////////////////////////////////////////////////////////////////////////////////
//// BaBOp

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum BaBOp {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
}

impl From<&Token> for BaBOp {
    fn from(tok: &Token) -> Self {
        match tok.name().as_str() {
            "<add>" => BaBOp::Add,
            "<sub>" => BaBOp::Sub,
            "<mul>" => BaBOp::Mul,
            "<div>" => BaBOp::Div,
            "<percent>" => BaBOp::Mod,
            _ => unreachable!(),
        }
    }
}

impl BaBOp {
    pub fn precedence(&self) -> usize {
        BOP_PREC_MAP.with(|bop_preced_map| {
            bop_preced_map.get(self).unwrap().clone()
        })
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaMod

/// Bare Lang 顶级作用域
#[derive(Debug)]
pub struct BaMod {
    pub scope: Rc<RefCell<BaScope>>,
    pub funtbl: Rc<IndexMap<BaFunKey, SymItem>>,
}


#[derive(Debug, Clone)]
pub enum BaStmt {
    Declare(BaDeclare),
    DefFun(BaDefFun),
    FunCall(BaFunCall),
    Block(Rc<RefCell<BaScope>>),
    // TwoAddr: Ignore
    IterBlock(BaIterBlock)
}

impl From<BaDeclare> for BaStmt {
    fn from(val: BaDeclare) -> Self {
        BaStmt::Declare(val)
    }
}

impl TryFrom<BaDecVal> for BaStmt {
    type Error = Box<dyn Error>;

    fn try_from(decval: BaDecVal) -> Result<Self, Self::Error> {
        Ok(match decval {
            BaDecVal::FunCall(funcall) => BaStmt::FunCall(funcall),
            BaDecVal::IterBlock(iterblock) => BaStmt::IterBlock(iterblock),
            _ => return Err(TrapCode::UnsupportedDecValToPri(&decval).emit_box_err()),
        })
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaDeclare

#[derive(Debug, Clone)]
pub struct BaDeclare {
    pub name: BaId,
    pub value: BaDecVal
}


/// Declared Value <=> LspExpr
#[derive(Debug, Clone)]
pub enum BaDecVal {
    PriVal(BaPriVal),
    FunCall(BaFunCall),
    IterBlock(BaIterBlock),
    // two operand should be same type
    TwoAddr(BaBOp, BaPriVal, BaPriVal)
}

impl GetBaType for BaDecVal {
    fn get_batype(&self) -> Option<BaType> {
        match &self {
            Self::FunCall(funcall) => Some(funcall.ret.clone()),
            Self::PriVal(prival) => prival.get_batype(),
            Self::TwoAddr(_bop, fstpti, _sndpri) => fstpti.get_batype(),
            Self::IterBlock(iterblk) => iterblk.get_batype()
        }
    }
}

impl GetLoc for BaDecVal {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::FunCall(funcall) => funcall.get_loc(),
            Self::PriVal(prival) => prival.get_loc(),
            Self::TwoAddr(_bop, fstpti, _sndpri) => {
                fstpti.get_loc()
            },
            Self::IterBlock(iterblk) => iterblk.srcloc.clone()
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//// BaDefFun

#[derive(Debug, Clone)]
pub struct BaDefFun {
    pub hdr: BaFunHdr,
    pub body: Rc<RefCell<BaScope>>
}


////////////////////////////////////////////////////////////////////////////////
//// BaIterBlock

#[derive(Debug, Clone)]
pub struct BaIterBlock {
    pub var_formal: String,
    pub var_outter: BaPriVal,
    pub body: Rc<RefCell<BaScope>>,
    pub srcloc: SrcLoc
}


impl GetBaType for BaIterBlock {
    fn get_batype(&self) -> Option<BaType> {
        Some(BaType::VoidUnit)
    }
}


////////////////////////////////////////////////////////////////////////////////
//// BaScope

#[derive(Clone)]
pub struct BaScope {
    pub(crate) id: usize,
    pub(crate) stmts: Vec<BaStmt>,
    pub(crate) tailval: Option<BaPriVal>,
    pub(crate) parent: Option<Rc<RefCell<Self>>>,
    pub(crate) symtbl: SymTbl
}

pub type SymTbl = IndexMap<String, SymItem>;

impl BaScope {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            stmts: vec![],
            tailval: None,
            parent: None,
            symtbl: indexmap! {}
        }
    }

    pub fn get_sym_item(&self, key: &str) -> Option<SymItem> {
        if let Some(item) = self.symtbl.get(key) {
            Some(item.clone())
        }
        else if let Some(paren) = &self.parent {
            paren.as_ref().borrow().get_sym_item(key)
        }
        else {
            None
        }
    }

    pub fn parent_id(&self) -> Option<usize> {
        if let Some(ref paren_scope_rc) = self.parent {
            Some(paren_scope_rc.as_ref().borrow().id)
        }
        else {
            None
        }
    }
}

impl fmt::Debug for BaScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f
        .debug_struct("BaScope")
        .field("id", &self.id)
        .field("stmts", &self.stmts)
        .field("tailval", &self.tailval)
        .field("parent", &self.parent_id())
        .field("symtbl", &self.symtbl)
        .finish()
    }
}


////////////////////////////////////////////////////////////////////////////////
//// SymItem

#[derive(Debug, Clone)]
pub struct SymItem {
    pub(crate) ty: BaType,
}


////////////////////////////////////////////////////////////////////////////////
//// External C Type

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTy {
    I64,
    I32,
    F64,
    CStr,  // char*
    Void
}

impl CTy {
    pub fn as_type(&self) -> BaType {
        match &self {
            Self::F64 => BaType::Float,
            Self::I64 => BaType::I64,
            Self::I32 => BaType::Int,
            Self::CStr => BaType::RawStr,
            Self::Void => BaType::VoidUnit
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExRefFunProto {
    pub name: String,
    pub params: Vec<CTy>,
    pub ret: CTy,
}
