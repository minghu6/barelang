//! Bare Lang Data

use std::rc::Rc;

#[derive(Debug)]
pub enum BaNum {
    USize(usize),
    ISize(isize),
    U8(u8),
    Float(f64)  // 64 bit float
}

#[derive(Debug)]
pub enum BaVal {
    Num(BaNum),
    Expr(Rc<BaExpr>),
    Unresolved
}

#[derive(Debug)]
pub struct BaId {
    pub name: String,
    pub value: BaVal,
    pub splid: Option<BaSplId>
}

#[derive(Debug)]
pub enum BaSplId {
    RS
}

#[derive(Debug)]
pub enum BaPri {
    Val(BaVal),
    Id(BaId)
}

#[derive(Debug)]
pub enum BaExpr {
    Pri(BaPri)
}

#[derive(Debug)]
pub enum BaStmt {
    Expr(BaExpr)
}

#[derive(Debug)]
pub enum BaBlockStmt {
    Stmt(BaStmt)
}

#[derive(Debug)]
pub struct BaBlockStmts {
    pub stmts: Vec<BaBlockStmt>
}

#[derive(Debug)]
pub enum AT {
    BlockStmts(BaBlockStmts)
}
