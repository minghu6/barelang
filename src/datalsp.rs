//! Syntax Data (Recursive)
#![allow(unused_imports)]

use std::{
    cell::{Ref, RefCell, RefMut},
    convert::TryInto,
    error::Error,
    fmt::Debug,
    rc::Rc,
};

use indexmap::IndexMap;

use crate::{datair::BaId, error::BaCErr, utils::Stack};
use crate::lexer::Token;
use crate::semantic_analyzer::BOP_PREC_MAP;
use crate::datair::{
    BaLit, BaSplId, BaBOp
};
////////////////////////////////////////////////////////////////////////////////
//// Bare Language Data Structure


#[derive(Debug, Clone)]
pub enum LspVal {
    Lit(BaLit),
    Expr(Rc<LspExpr>),
    Unresolved,
}

#[derive(Debug, Clone)]
pub struct LspId {
    pub name: String,
    pub splid: Option<BaSplId>,

    pub value: LspVal,
}

impl LspId {
    pub fn sym(&self) -> String {
        if let Some(splid) = &self.splid {
            format!("{:?}#{}", splid, self.name)
        }
        else {
            self.name.clone()
        }
    }
}

impl LspId {
    pub fn as_baid(self) -> BaId {
        BaId::from(self)
    }
}

#[derive(Debug, Clone)]
pub enum LspPri {
    Val(LspVal),
    Id(Rc<RefCell<LspId>>),
    Expr(Rc<RefCell<LspExpr>>)
}

#[derive(Debug)]
pub enum LspExpr {
    Pri(LspPri),
    FunCall(LspFunCall),

    // Pri is just a special case of CompPri, We do for flatten structure
    CompPri(LspCompPriTN),
    TwoPri(BaBOp, LspPri, LspPri)
}


#[derive(Debug, Clone)]
pub enum LspCompPriTN {
    Tree(Rc<RefCell<LspCompPriT>>),
    Leaf(Rc<RefCell<LspPri>>)
}

impl LspCompPriTN {
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

    pub fn as_leaf(&self) -> Option<&Rc<RefCell<LspPri>>> {
        match self {
            Self::Leaf(leaf) => Some(leaf),
            _ => None
        }
    }

    pub fn as_tree(&self) -> Option<&Rc<RefCell<LspCompPriT>>> {
        match self {
            Self::Tree(tree) => Some(tree),
            _ => None
        }
    }
}


#[derive(Debug)]
pub struct LspCompPriT {
    pub bop: BaBOp,
    pub lf: LspCompPriTN,
    pub rh: LspCompPriTN
}


#[derive(Debug)]
pub enum LspStmt {
    Expr(LspExpr),
    Empty, // semi
}

#[derive(Debug)]
pub struct LspFunCall {
    pub name: BaId,
    pub args: Vec<LspExpr>,
}

#[derive(Debug)]
pub enum LspBlockStmt {
    Stmt(LspStmt),
    Declare(LspId),
}

#[derive(Debug, Clone)]
pub struct LspBlockStmtRef(Rc<RefCell<LspBlockStmt>>);

impl LspBlockStmtRef {
    pub fn new(bablockstmt: LspBlockStmt) -> Self {
        Self(Rc::new(RefCell::new(bablockstmt)))
    }

    pub fn block_stmt_ref(&self) -> Ref<LspBlockStmt> {
        self.0.as_ref().borrow()
    }

    pub fn block_stmt_ref_mut(&self) -> RefMut<LspBlockStmt> {
        self.0.as_ref().borrow_mut()
    }
}

#[derive(Debug)]
pub enum ModuleLisp {
    BlockStmts(Vec<LspBlockStmtRef>),
}


////////////////////////////////////////////////////////////////////////////////
//// Stack Frame

#[derive(Debug)]
pub struct _StackFrame {
    pub syms: IndexMap<String, LspId>,
    pub blockstmts: Vec<LspBlockStmtRef>,
    pub paren: Option<StackFrame>,
    pub children: Vec<StackFrame>,
}

/// _StackFrame Rc<RefCell<T>> Wrapper
#[derive(Clone)]
pub struct StackFrame(Rc<RefCell<_StackFrame>>);

impl StackFrame {
    pub fn new(
        syms: IndexMap<String, LspId>,
        blockstmts: Vec<LspBlockStmtRef>,
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

    pub fn get_sym(&self, idname: &str) -> Option<LspId> {
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
