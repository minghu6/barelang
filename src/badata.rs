//! Bare Lang Data
#![allow(unused_imports)]

use std::{
    cell::{Ref, RefCell, RefMut},
    convert::TryInto,
    error::Error,
    fmt::Debug,
    rc::Rc,
};

use indexmap::IndexMap;
use inkwell::{context::Context, types::FunctionType};

use crate::{error::BaCErr, utils::Stack};
use crate::lexer::Token;
use crate::semantic_analyzer::BOP_PREC_MAP;

////////////////////////////////////////////////////////////////////////////////
//// Bare Language Data Structure

#[derive(Debug, Clone)]
pub enum BaNum {
    USize(usize),
    ISize(isize),
    I64(i64),
    U8(u8),
    Float(f64), // 64 bit float
}

#[derive(Debug, Clone)]
pub enum BaVal {
    Num(BaNum),
    Expr(Rc<BaExpr>),
    Unresolved,
}

#[derive(Debug, Clone)]
pub struct BaId {
    pub name: String,
    pub splid: Option<BaSplId>,

    pub value: BaVal,
}

#[derive(Debug, Clone)]
pub enum BaSplId {
    RS,
}

#[derive(Debug, Clone)]
pub enum BaPri {
    Val(BaVal),
    Id(Rc<RefCell<BaId>>),
    Expr(Rc<RefCell<BaExpr>>)
}

#[derive(Debug)]
pub enum BaExpr {
    Pri(BaPri),
    FunCall(BaFunCall),

    // Pri is just a special case of CompPri, We do for flatten structure
    CompPri(BaCompPriTN),
    TwoPri(BaBOp, BaPri, BaPri)
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


#[derive(Debug, Clone)]
pub enum BaCompPriTN {
    Tree(Rc<RefCell<BaCompPriT>>),
    Leaf(Rc<RefCell<BaPri>>)
}

impl BaCompPriTN {
    pub fn is_tree(&self) -> bool {
        match self {
            &Self::Tree(_) => true,
            _ => false
        }
    }

    pub fn is_leaf(&self) -> bool {
        match self {
            &Self::Leaf(_) => true,
            _ => false
        }
    }

    pub fn as_leaf(&self) -> Option<&Rc<RefCell<BaPri>>> {
        match self {
            Self::Leaf(leaf) => Some(leaf),
            _ => None
        }
    }

    pub fn as_tree(&self) -> Option<&Rc<RefCell<BaCompPriT>>> {
        match self {
            Self::Tree(tree) => Some(tree),
            _ => None
        }
    }
}


#[derive(Debug)]
pub struct BaCompPriT {
    pub bop: BaBOp,
    pub lf: BaCompPriTN,
    pub rh: BaCompPriTN
}


#[derive(Debug)]
pub enum BaStmt {
    Expr(BaExpr),
    Empty, // semi
}

#[derive(Debug)]
pub struct BaFunCall {
    pub name: BaId,
    pub args: Vec<BaExpr>,
}

#[derive(Debug)]
pub enum BaBlockStmt {
    Stmt(BaStmt),
    Declare(BaId),
}

#[derive(Debug, Clone)]
pub struct BaBlockStmtRef(Rc<RefCell<BaBlockStmt>>);

impl BaBlockStmtRef {
    pub fn new(bablockstmt: BaBlockStmt) -> Self {
        Self(Rc::new(RefCell::new(bablockstmt)))
    }

    pub fn block_stmt_ref(&self) -> Ref<BaBlockStmt> {
        self.0.as_ref().borrow()
    }

    pub fn block_stmt_ref_mut(&self) -> RefMut<BaBlockStmt> {
        self.0.as_ref().borrow_mut()
    }
}

#[derive(Debug)]
pub enum ModuleLisp {
    BlockStmts(Vec<BaBlockStmtRef>),
}

pub enum ExRefType {
    I64,
    F64,
}

pub struct ExRefFunProto {
    pub name: String,
    pub funtype_getter: fn(ctx: &Context) -> FunctionType,
}

////////////////////////////////////////////////////////////////////////////////
//// Stack Frame

#[derive(Debug)]
pub struct _StackFrame {
    pub syms: IndexMap<String, BaId>,
    pub blockstmts: Vec<BaBlockStmtRef>,
    pub paren: Option<StackFrame>,
    pub children: Vec<StackFrame>,
}

/// _StackFrame Rc<RefCell<T>> Wrapper
#[derive(Clone)]
pub struct StackFrame(Rc<RefCell<_StackFrame>>);

impl StackFrame {
    pub fn new(
        syms: IndexMap<String, BaId>,
        blockstmts: Vec<BaBlockStmtRef>,
        paren: Option<Self>,
        children: Vec<Self>,
    ) -> Self {
        Self(Rc::new(RefCell::new(_StackFrame {
            syms,
            blockstmts,
            paren,
            children,
        })))
    }

    pub fn paren(&self) -> Option<Self> {
        self.0.as_ref().borrow().paren.clone()
    }

    pub fn frame_ref(&self) -> Ref<_StackFrame> {
        self.0.as_ref().borrow()
    }

    pub fn frame_ref_mut(&self) -> RefMut<_StackFrame> {
        self.0.as_ref().borrow_mut()
    }

    pub fn get_sym(&self, idname: &str) -> Option<BaId> {
        let frame_ref = self.frame_ref();

        if let Some(baid) = frame_ref.syms.get(idname) {
            Some(baid.clone())
        } else if let Some(paren_frame) = &frame_ref.paren {
            paren_frame.get_sym(idname)
        } else {
            None
        }
    }
}

impl Debug for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.frame_ref())
        } else {
            write!(f, "{:?}", self.frame_ref())
        }
    }
}
