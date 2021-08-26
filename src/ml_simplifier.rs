#![allow(unused_imports)]

use std::rc::Rc;
use std::cell::{
    RefCell, Ref,
};

use uuid::Uuid;
use itertools::Itertools;
use indexmap::{IndexMap, indexmap};

use crate::datalsp::*;
use crate::datair::*;
use crate::rslib::search_rs_lib;
use crate::utils::{
    gen_counter, CounterType
};


#[derive(Debug, Clone)]
struct SymItem {
    ty: BaType
}

pub struct MLSimplifier {
    symtbl: IndexMap<String, SymItem>
}

impl MLSimplifier {
    pub fn new() -> Self {
        Self {
            symtbl: IndexMap::new()
        }
    }

    pub fn simplify_ml(&mut self, ml: ModuleLisp) -> BaBin {
        let mut stmts = vec![];

        match ml {
            ModuleLisp::BlockStmts(blkstmtsref_vec) => {
                for blkstmtsref in blkstmtsref_vec {
                    stmts.extend(
                        self.simplify_blkstmt(&blkstmtsref.block_stmt_ref())
                    );
                }

                BaBin {
                    stmts
                }
            }
        }
    }


    fn simplify_blkstmt(&mut self, blkstmtsref: &LspBlockStmt) -> Vec<BaStmt> {
        match blkstmtsref {
            LspBlockStmt::Stmt(lspstmt) => {
                self.simplify_stmt(lspstmt)
            },
            LspBlockStmt::Declare(lspid) => {
                self.simplify_declare(lspid)
            }
        }
    }

    fn simplify_stmt(&self, lspstmt: &LspStmt) -> Vec<BaStmt> {
        match lspstmt {
            LspStmt::Expr(lspexpr) => {
                self.simplify_expr(lspexpr).1
            },
            LspStmt::Empty => {
                vec![]
            }
        }
    }

    fn simplify_declare(&mut self, lspid: &LspId) -> Vec<BaStmt> {
        let (value, mut new_stmts) = self.simplify_value(&lspid.value);

        self.symtbl.insert(lspid.sym(), SymItem { ty: value.to_type() });

        new_stmts.push(
        BaStmt::Declare(
            BaDeclare {
                name: BaId {
                    name: lspid.name.clone(),
                    splid: lspid.splid.clone(),
                    ty: Some(value.to_type())
                },
                value
            })
        );

        new_stmts
    }

    fn simplify_value(&self, lspval: &LspVal) -> (BaDecVal, Vec<BaStmt>) {
        match lspval {
            LspVal::Lit(lit) => {
                let decval = BaDecVal::PriVal(
                    BaPriVal::Lit(lit.clone())
                );

                (decval, vec![])
            },
            LspVal::Expr(expr) => {
                let (lastid, new_stmts) = self.simplify_expr(expr);
                let decval = BaDecVal::PriVal(
                    BaPriVal::Id(lastid)
                );

                (decval, new_stmts)
            },
            _ => {
                unreachable!()
            }
        }

    }

    fn simplify_expr(&self, lspexpr: &LspExpr) -> (BaId, Vec<BaStmt>) {
        match lspexpr {
            LspExpr::Pri(pri) => {
                self.simplify_pri(pri)
            },
            LspExpr::TwoPri(bop, lspfstpri, lspsndpri) => {
                let (newfstid, mut newfststmts, )
                = self.simplify_pri(lspfstpri);

                let (newsndid, newsndstmts, )
                 = self.simplify_pri(lspsndpri);

                newfststmts.extend(newsndstmts.into_iter());

                let decval
                = BaDecVal::TwoAddr(
                    bop.clone(),
                    BaPriVal::Id(newfstid),
                    BaPriVal::Id(newsndid)
                );
                let decid = BaId {
                    name: gensym_rand(),
                    splid: None,
                    ty: Some(decval.to_type())
                };

                let dec_stmt = BaStmt::Declare(
                    BaDeclare {
                        name: decid.clone(),
                        value: decval
                    }
                );

                newfststmts.push(dec_stmt);

                (decid, newfststmts)
            },
            LspExpr::FunCall(lspfuncall) => {
                self.simplify_funcall(lspfuncall)
            },
            LspExpr::CompPri(lspcomptn) => {
                self.simplify_compexpr(lspcomptn)
            }
        }
    }

    fn simplify_pri(&self, lsppri: &LspPri) -> (BaId, Vec<BaStmt>) {
        match lsppri {
            LspPri::Expr(lspexpr_rc) => {
                self.simplify_expr(&lspexpr_rc.as_ref().borrow())
            },
            LspPri::Id(lspid_rc) => {
                let id = self.simplify_id(&lspid_rc.as_ref().borrow());
                // TODO: type annotation
                (id, vec![])
            },
            LspPri::Val(lspval) => {
                let (val, mut new_stmts) = self.simplify_value(lspval);
                let newsym = gensym_rand();
                let newid = BaId {
                    name: newsym,
                    splid: None,
                    ty: Some(val.to_type())
                };
                let dec_stmt = BaStmt::Declare(
                    BaDeclare {
                        name: newid.clone(),
                        value: val
                    }
                );
                new_stmts.push(dec_stmt);

                (newid, new_stmts)
            }
        }
    }

    fn simplify_id(&self, lspid: &LspId) -> BaId {
        let ty = if let Some(symitem) = self.symtbl.get(&lspid.sym()) {
            Some(symitem.ty.clone())
        }
        else {
            None
        };

        BaId {
            name: lspid.name.clone(),
            splid: lspid.splid.clone(),
            ty
        }
    }

    fn simplify_funcall(&self, lspfuncall: &LspFunCall) -> (BaId, Vec<BaStmt>) {
        let mut new_stmts = vec![];
        let mut new_args = vec![];

        for arg in lspfuncall.args.iter() {
            let (newid, arg_stmts) = self.simplify_expr(arg);
            new_stmts.extend(arg_stmts.into_iter());
            new_args.push(BaPriVal::Id(newid));
        }

        let new_funcall = BaFunCall {
            name: lspfuncall.name.clone(),
            args: new_args
        };

        new_stmts.push(BaStmt::FunCall(new_funcall));

        (lspfuncall.name.clone(), new_stmts)
    }



    fn simplify_compexpr(&self, pritn: &LspCompPriTN) -> (BaId, Vec<BaStmt>) {
        let mut gensym_ser = gen_gensym_ser("tmp", gen_counter());

        self.unfold_compexpr(pritn, &mut gensym_ser)
    }

    fn unfold_compexpr(&self, pritn: &LspCompPriTN, gensym_ser: &mut SymGen) -> (BaId, Vec<BaStmt>) {
        match pritn {
            LspCompPriTN::Tree(tree_rc) => {
                let tree_ref = tree_rc.as_ref().borrow();

                let (lf_id, mut lf_stmts)
                = self.unfold_compexpr(&tree_ref.lf, gensym_ser);

                let (rh_id, rh_stmts)
                = self.unfold_compexpr(&tree_ref.rh, gensym_ser);

                /* build declare expression */

                let decval = BaDecVal::TwoAddr(
                    tree_ref.bop.clone(),
                    BaPriVal::Id(lf_id),
                    BaPriVal::Id(rh_id),
                );
                let decid = BaId {
                    name: gensym_ser(),
                    splid: None,
                    ty: Some(decval.to_type())
                };

                let decstmt = BaStmt::Declare(
                    BaDeclare {
                        name: decid.clone(),
                        value: decval
                    }
                );

                lf_stmts.extend(rh_stmts.into_iter());
                lf_stmts.push(decstmt);

                (decid, lf_stmts)
            },
            LspCompPriTN::Leaf(lsppri_rc) => {
                let (pri_id, new_stmts)
                = self.simplify_pri(&lsppri_rc.as_ref().borrow());

                (pri_id, new_stmts)
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Bare Lang semantics analyze step 1:- Ref Resolver



////////////////////////////////////////////////////////////////////////////////
//// Helper Functions

pub fn gensym_rand() -> String {
    format!("__{}", Uuid::new_v4().to_simple())
}

pub type SymGen = impl FnMut() -> String;

pub fn gen_gensym_ser(base: &str, mut counter: CounterType) -> SymGen {
    let base = base.to_string();

    move || format!("{}_{}_{}", base, counter(), gensym_rand())
}


#[cfg(test)]
mod test {

    #[test]
    fn test_gensym() {
        use super::{gen_gensym_ser, gensym_rand};
        use crate::utils::gen_counter;

        println!("randsym: {}", gensym_rand());
        println!("randsym: {}", gensym_rand());

        let counter = gen_counter();
        let mut symgen = gen_gensym_ser("t", counter);

        println!("sersym: {}", symgen());
        println!("sersym: {}", symgen());
    }
}
