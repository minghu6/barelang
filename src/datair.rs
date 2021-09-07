//! IR Data (Non Recursive)

use crate::datalsp::LspId;
use crate::lexer::{SrcLoc, Token};
use crate::semantic_analyzer::BOP_PREC_MAP;

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
}

impl ToBaType for BaLit {
    fn to_batype(&self) -> BaType {
        match self {
            &Self::Float(_) => BaType::Float,
            &Self::I32(_) => BaType::I32,
            &Self::I64(_) => BaType::I64,
            &Self::USize(_) => BaType::USize,
            &Self::U8(_) => BaType::U8
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
            Self::U8(u8) => u8.loc.clone()
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaType

#[derive(Debug, Clone)]
pub enum BaType {
    USize,
    I64,
    I32,
    U8,
    Float,
    Void,   // Rust Unit,
    String,
    ExRefFunProto(ExRefFunProto)
}

////////////////////////////////////////////////////////////////////////////////
//// BaPriVal

#[derive(Debug)]
pub enum BaPriVal {
    Lit(BaLit),
    Id(BaId),
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

        }
    }
}

impl GetLoc for BaPriVal {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Lit(lit) => lit.get_loc(),
            Self::Id(id) => id.get_loc()
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaFunCall

#[derive(Debug)]
pub struct BaFunCall {
    pub name: BaId,
    pub args: Vec<BaPriVal>
}

impl BaFunCall {
    pub fn get_ret_type(&self) -> Option<BaType> {
        if let Some(ty) = &self.name.ty {
            match ty {
                BaType::ExRefFunProto(exref_proto) => {
                    Some(exref_proto.ret.as_type())
                },
                _ => unreachable!()
            }
        }
        else {
            None
        }
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

impl GetLoc for BaId {
    fn get_loc(&self) -> SrcLoc {
        self.loc.clone()
    }
}

impl From<LspId> for BaId {
    fn from(lspid: LspId) -> Self {
        Self {
            name: lspid.name,
            splid: lspid.splid,
            ty: None,
            loc: lspid.loc
        }
    }
}


#[derive(Debug, Clone)]
pub enum BaSplId {
    RS,
}

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
        match tok.name() {
            "add" => BaBOp::Add,
            "sub" => BaBOp::Sub,
            "mul" => BaBOp::Mul,
            "div" => BaBOp::Div,
            "percent" => BaBOp::Mod,
            _ => unreachable!(),
        }
    }
}

impl BaBOp {
    pub fn precedence(&self) -> usize {
        BOP_PREC_MAP.get(self).unwrap().clone()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaBin

/// 变量作用域
#[derive(Debug)]
pub struct BaBin {
    pub stmts: Vec<BaStmt>
}


#[derive(Debug)]
pub enum BaStmt {
    Declare(BaDeclare),
    FunCall(BaFunCall)
}

#[derive(Debug)]
pub struct BaDeclare {
    pub name: BaId,
    pub value: BaDecVal
}

/// Declared Value <=> LspExpr
#[derive(Debug)]
pub enum BaDecVal {
    PriVal(BaPriVal),
    FunCall(BaFunCall),

    /// two operand should be same type
    TwoAddr(BaBOp, BaPriVal, BaPriVal)
}

impl BaDecVal {
    pub fn to_type(&self) -> BaType {
        match &self {
            Self::FunCall(funcall) => {
                funcall.get_ret_type().unwrap().clone()
            },
            Self::PriVal(prival) => {
                prival.get_batype().unwrap().clone()
            },
            Self::TwoAddr(_bop, fst, _snd) => {
                fst.get_batype().unwrap().clone()
            }
        }
    }
}

impl GetBaType for BaDecVal {
    fn get_batype(&self) -> Option<BaType> {
        match &self {
            Self::FunCall(funcall) => funcall.get_ret_type(),
            Self::PriVal(prival) => prival.get_batype(),
            Self::TwoAddr(_bop, fstpti, _sndpri) => fstpti.get_batype()
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
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// External Reference

#[derive(Debug, Clone)]
pub enum ExRefType {
    I64,
    F64,
    String,
    Void
}

impl ExRefType {
    pub fn as_type(&self) -> BaType {
        match &self {
            Self::F64 => BaType::Float,
            Self::I64 => BaType::I64,
            Self::String => BaType::String,
            Self::Void => BaType::Void
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExRefFunProto {
    pub name: String,
    pub params: Vec<ExRefType>,
    pub ret: ExRefType,
}


