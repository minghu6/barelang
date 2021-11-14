//! Syntax Data (Recursive)
#![allow(unused_imports)]

use std::{
    cell::{Ref, RefCell, RefMut},
    convert::TryInto,
    error::Error,
    fmt::Debug,
    rc::Rc,
};

use bacommon::lexer::SrcLoc;
use indexmap::IndexMap;
use regex::NoExpand;

use crate::error::BaCErr;
use crate::frontend::lexer::{
    Token
};
use crate::middleware::datair::*;


////////////////////////////////////////////////////////////////////////////////
//// ModuleLisp

#[derive(Debug)]
pub enum ModuleLisp {
    BlockStmts(LspBlockStmts)
}


////////////////////////////////////////////////////////////////////////////////
//// LspBlockStmt/LspBlockStmtRef

#[derive(Debug)]
pub struct LspBlockStmts {
    pub block_stmts: Vec<LspBlockStmtRef>,
    pub tail_expr: Option<LspExpr>
}

impl LspBlockStmts {
    pub fn empty() -> Self {
        Self {
            block_stmts: vec![],
            tail_expr: None
        }
    }
}




////////////////////////////////////////////////////////////////////////////////
//// LspBlockStmt/LspBlockStmtRef

#[derive(Debug)]
pub enum LspBlockStmt {
    Item(LspItem),
    Stmt(LspStmt),
    Block(LspBlock)
}

#[derive(Debug, Clone)]
pub struct LspBlockStmtRef(Rc<RefCell<LspBlockStmt>>);

impl LspBlockStmtRef {
    pub fn new(lspblockstmt: LspBlockStmt) -> Self {
        Self(Rc::new(RefCell::new(lspblockstmt)))
    }

    pub fn block_stmt_ref(&self) -> Ref<LspBlockStmt> {
        self.0.as_ref().borrow()
    }

    pub fn block_stmt_ref_mut(&self) -> RefMut<LspBlockStmt> {
        self.0.as_ref().borrow_mut()
    }
}


////////////////////////////////////////////////////////////////////////////////
//// LspItem

#[derive(Debug)]
pub enum LspItem {
    DefFun(LspDefFun)
}


////////////////////////////////////////////////////////////////////////////////
//// LspDefFun

#[derive(Debug)]
pub struct LspDefFun {
    pub hdr: BaFunHdr,
    pub body: LspBlock
}

impl From<(BaId, Vec<BaParam>, BaType, LspBlock)> for LspDefFun {
    fn from(quadruple: (BaId, Vec<BaParam>, BaType, LspBlock)) -> Self {
        let (id, params, ret, body) = quadruple;

        let hdr = BaFunHdr::from((id, params, ret));

        Self {
            hdr,
            body
        }
    }
}

impl GetLoc for LspDefFun {
    fn get_loc(&self) -> SrcLoc {
        self.hdr.get_loc()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// LspStmt

#[derive(Debug)]
pub enum LspStmt {
    Expr(LspExpr),
    Declare(Rc<LspDeclare>),  // Rc to avoid cycle dependency
    Empty,                    // semi
}


////////////////////////////////////////////////////////////////////////////////
//// LspBlock

#[derive(Debug)]
pub struct LspBlock {
    pub block_stmts: LspBlockStmts
}


////////////////////////////////////////////////////////////////////////////////
//// LspExpr

#[derive(Debug, Clone)]
pub enum LspExpr {
    Pri(LspPri),
    FunCall(LspFunCall),
    IterBlock(LspIterBlock),
    TwoPri(LspBOp, Rc<LspExpr>, Rc<LspExpr>),
    Range(LspRange)
}

impl GetLoc for LspExpr  {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::FunCall(funcall) => {
                funcall.get_loc()
            },
            Self::Pri(pri) => {
                pri.get_loc()
            },
            Self::TwoPri(_bop, fstpri, _sndpri) => {
                fstpri.get_loc()
            },
            Self::IterBlock(iterblock) => iterblock.srcloc.clone(),
            Self::Range(range) => {
                range.srcloc.clone()
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// LspRange

#[derive(Debug, Clone)]
pub struct LspRange {
    pub start: Option<LspPri>,
    pub end: Option<LspPri>,
    pub srcloc: SrcLoc
}



////////////////////////////////////////////////////////////////////////////////
//// LspPri

#[derive(Debug, Clone)]
pub enum LspPri {
    Lit(BaLit),
    Id(Rc<BaId>),
    Expr(Rc<LspExpr>),
    Vector(LspVector),
}

impl GetLoc for LspPri {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Expr(expr_rc) => {
                expr_rc.as_ref().get_loc()
            },
            Self::Lit(lit) => {
                lit.get_loc()
            },
            Self::Id(id_rc) => {
                id_rc.as_ref().loc.clone()
            },
            Self::Vector(vec) => {
                vec.srcloc.clone()
            }
        }
    }
}

impl LspPri {
    pub fn to_lspid(&self) -> Option<Rc<BaId>> {
        if let Self::Id(id_rc) = self {
            Some(id_rc.clone())
        }
        else {
            None
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//// LspIterBlock

#[derive(Debug, Clone)]
pub struct LspIterBlock {
    pub var_formal: String,
    pub var_outter: Box<LspPri>,
    pub ctrl_body: Rc<LspBlock>,
    pub srcloc: SrcLoc
}



////////////////////////////////////////////////////////////////////////////////
//// LspLit

#[derive(Debug, Clone)]
pub struct LspLit {
    pub name: String,
    pub splid: Option<BaSplId>,

    pub loc: SrcLoc
}



////////////////////////////////////////////////////////////////////////////////
//// LspDeclare

#[derive(Debug)]
pub struct LspDeclare {
    pub id: BaId,
    pub val: LspExpr
}

impl GetLoc for LspDeclare {
    fn get_loc(&self) -> SrcLoc {
        self.id.loc.clone()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// LspFunCall

#[derive(Debug, Clone)]
pub struct LspFunCall {
    pub name: BaId,
    pub args: Vec<LspExpr>,
}

impl LspFunCall {
    pub fn name(&self) -> String {
        self.name.name.to_owned()
    }
}

impl GetLoc for LspFunCall {
    fn get_loc(&self) -> SrcLoc {
        self.name.loc.clone()
    }
}


////////////////////////////////////////////////////////////////////////////////
//// LspBOp

type LspBOp = BaBOp;



////////////////////////////////////////////////////////////////////////////////
//// LspVector

#[derive(Debug, Clone)]
pub struct LspVector {
    pub splid: BaSplId,
    pub elems: Vec<LspExpr>,
    pub srcloc: SrcLoc
}

