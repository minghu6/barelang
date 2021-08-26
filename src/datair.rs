//! IR Data (Non Recursive)

use crate::datalsp::LspId;
use crate::lexer::Token;
use crate::semantic_analyzer::BOP_PREC_MAP;


pub trait ToBaType {
    fn to_batype(&self) -> BaType;
}

pub trait GetBaType {
    fn get_batype(&self) -> Option<BaType>;
}


///
/// Default Int Type [reference this](https://github.com/rust-lang/rfcs/blob/master/text/0212-restore-int-fallback.md#rationale-for-the-choice-of-defaulting-to-i32)
///
#[derive(Debug, Clone)]
pub enum BaLit {
    USize(usize),
    I64(i64),
    I32(i32),
    U8(u8),
    Float(f64), // 64 bit float
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

#[derive(Debug, Clone)]
pub enum BaType {
    USize,
    I64,
    I32,
    U8,
    Float,
    Void,   // Rust Unit,
    ExRefFunProto(ExRefFunProto)
}


/// Primary Value
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

#[derive(Debug, Clone)]
pub struct BaId {
    pub name: String,
    pub splid: Option<BaSplId>,
    pub ty: Option<BaType>
}

impl BaId {
    pub fn with_name(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            splid: None,
            ty: None
        }
    }
}

impl From<LspId> for BaId {
    fn from(lspid: LspId) -> Self {
        Self {
            name: lspid.name.clone(),
            splid: lspid.splid.clone(),
            ty: None
        }
    }
}


#[derive(Debug, Clone)]
pub enum BaSplId {
    RS,
}

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

/// Declared Value
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


////////////////////////////////////////////////////////////////////////////////
//// External Reference

#[derive(Debug, Clone)]
pub enum ExRefType {
    I64,
    F64,
    Void
}

impl ExRefType {
    pub fn as_type(&self) -> BaType {
        match self {
            &Self::F64 => BaType::Float,
            &Self::I64 => BaType::I64,
            &Self::Void => BaType::Void
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExRefFunProto {
    pub name: String,
    pub params: Vec<ExRefType>,
    pub ret: ExRefType,
}


