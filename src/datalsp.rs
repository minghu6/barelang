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

use crate::utils::Stack;
use crate::datair::{
    BaId, GetLoc
};
use crate::error::BaCErr;
use crate::lexer::{
    Token, SrcLoc
};
use crate::semantic_analyzer::BOP_PREC_MAP;
use crate::datair::{
    BaLit, BaSplId, BaBOp
};

////////////////////////////////////////////////////////////////////////////////
//// Bare Language Data Structure

#[derive(Debug, Clone)]
pub struct LspId {
    pub name: String,
    pub splid: Option<BaSplId>,

    pub loc: SrcLoc
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
    Lit(BaLit),
    Id(Rc<RefCell<LspId>>),
    Expr(Rc<RefCell<LspExpr>>)
}

impl GetLoc for LspPri {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Expr(expr_rc) => {
                expr_rc.as_ref().borrow().get_loc()
            },
            Self::Lit(lit) => {
                lit.get_loc()
            },
            Self::Id(id_rc) => {
                id_rc.as_ref().borrow().loc.clone()
            }
        }
    }
}

#[derive(Debug)]
pub enum LspExpr {
    Pri(LspPri),
    FunCall(LspFunCall),

    // Pri is just a special case of CompPri, We do for flatten structure
    CompPri(LspCompPriTN),
    TwoPri(BaBOp, LspPri, LspPri)
}

impl GetLoc for LspExpr  {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::CompPri(comppri) => {
                comppri.get_loc()
            },
            Self::FunCall(funcall) => {
                funcall.get_loc()
            },
            Self::Pri(pri) => {
                pri.get_loc()
            },
            Self::TwoPri(_bop, fstpri, _sndpri) => {
                fstpri.get_loc()
            }
        }
    }
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


impl GetLoc for LspCompPriTN {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Leaf(pri_rc) => {
                pri_rc.as_ref().borrow().get_loc()
            },
            Self::Tree(tree_rc) => {
                tree_rc.as_ref().borrow().get_loc()
            }
        }
    }
}

#[derive(Debug)]
pub struct LspCompPriT {
    pub bop: BaBOp,
    pub lf: LspCompPriTN,
    pub rh: LspCompPriTN
}

impl LspCompPriT {
    fn first_pri(tree_rc: Rc<RefCell<LspCompPriT>>) -> Rc<RefCell<LspPri>> {
        let tree_ref = tree_rc.as_ref().borrow();

        match &tree_ref.lf {
            LspCompPriTN::Tree(_tree_ref) => {
                Self::first_pri(tree_rc.clone())
            },
            LspCompPriTN::Leaf(pri_rc) => pri_rc.clone(),
        }
    }
}

impl GetLoc for LspCompPriT {
    fn get_loc(&self) -> SrcLoc {
        match &self.lf {
            LspCompPriTN::Tree(lftree_rc) => {
                let fstpri_rc = Self::first_pri(lftree_rc.clone());
                let fstpri_ref = fstpri_rc.as_ref().borrow();
                fstpri_ref.get_loc()
            },
            LspCompPriTN::Leaf(leaf_rc) => {
                leaf_rc.as_ref().borrow().get_loc()
            },
        }
    }
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

impl GetLoc for LspFunCall {
    fn get_loc(&self) -> SrcLoc {
        self.name.loc.clone()
    }
}


#[derive(Debug)]
pub enum LspBlockStmt {
    Stmt(LspStmt),
    Declare(LspId, LspExpr),
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
