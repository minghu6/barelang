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
            LspBlockStmt::Declare(id, expr) => {
                self.simplify_declare(id, expr)
                .into_iter().map(|dec| BaStmt::Declare(dec)).collect_vec()
            }
        }
    }

    fn simplify_stmt(&self, lspstmt: &LspStmt) -> Vec<BaStmt> {
        match lspstmt {
            LspStmt::Expr(lspexpr) => {
                let (decval, new_decs) = self.simplify_expr(lspexpr);
                let mut stmts = new_decs
                .into_iter()
                .map(|dec| BaStmt::Declare(dec))
                .collect_vec();

                if let Some(decstmt) = self.decval2stmt(decval) {
                    stmts.push(decstmt);
                }

                stmts
            },
            LspStmt::Empty => {
                vec![]
            }
        }
    }

    fn simplify_declare(&mut self, lspid: &LspId, lspexpr: &LspExpr) -> Vec<BaDeclare> {
        let (decval, mut new_decs)
        = self.simplify_expr(lspexpr);

        self.symtbl.insert(lspid.sym(), SymItem { ty: decval.get_batype().unwrap() });

        let ty = decval.get_batype();
        let loc = decval.get_loc();
        let prival = self.decval2prival(decval, &mut new_decs);

        new_decs.push(
            BaDeclare {
                name: BaId {
                    name: lspid.name.clone(),
                    splid: lspid.splid.clone(),
                    ty,
                    loc
                },
                value: BaDecVal::PriVal(prival)
            }
        );

        new_decs
    }


    fn simplify_expr(&self, lspexpr: &LspExpr) -> (BaDecVal, Vec<BaDeclare>) {
        match lspexpr {
            LspExpr::Pri(pri) => {
                self.simplify_pri(pri)
            },
            LspExpr::TwoPri(bop, lspfstpri, lspsndpri) => {
                let (newfstdecval, mut newfstdecs, )
                = self.simplify_pri(lspfstpri);

                let (newsnddecval, newsndstmts, )
                 = self.simplify_pri(lspsndpri);

                newfstdecs.extend(newsndstmts.into_iter());

                let ty = newfstdecval.get_batype();
                let loc = newfstdecval.get_loc();

                let fstpri
                = self.decval2prival(newfstdecval, &mut newfstdecs);

                let sndpri
                = self.decval2prival(newsnddecval, &mut newfstdecs);

                let decval
                = BaDecVal::TwoAddr(
                    bop.clone(),
                    fstpri,
                    sndpri
                );

                let decid = BaId {
                    name: gensym_rand(),
                    splid: None,
                    ty,
                    loc
                };

                let dec = BaDeclare {
                    name: decid.clone(),
                    value: decval
                };

                newfstdecs.push(dec);

                (BaDecVal::PriVal(BaPriVal::Id(decid)), newfstdecs)
            },
            LspExpr::FunCall(lspfuncall) => {
                self.simplify_funcall(lspfuncall)
            },
            LspExpr::CompPri(lspcomptn) => {
                let (prival, new_decs)
                = self.simplify_compexpr(lspcomptn);

                (BaDecVal::PriVal(prival), new_decs)
            }
        }
    }

    fn simplify_pri(&self, lsppri: &LspPri) -> (BaDecVal, Vec<BaDeclare>) {
        match lsppri {
            LspPri::Expr(lspexpr_rc) => {
                self.simplify_expr(&lspexpr_rc.as_ref().borrow())
            },
            LspPri::Id(lspid_rc) => {
                let id = self.simplify_id(&lspid_rc.as_ref().borrow());

                (BaDecVal::PriVal(BaPriVal::Id(id)), vec![])
            },
            LspPri::Lit(lit) => {
                (BaDecVal::PriVal(BaPriVal::Lit(lit.clone())), vec![])
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
            ty,
            loc: lspid.loc.clone()
        }
    }

    fn simplify_funcall(&self, lspfuncall: &LspFunCall)
    -> (BaDecVal, Vec<BaDeclare>)
    {
        let mut new_decs = vec![];
        let mut new_args = vec![];

        for arg in lspfuncall.args.iter() {
            let (decval, arg_decs) = self.simplify_expr(arg);
            new_decs.extend(arg_decs.into_iter());

            match decval {
                BaDecVal::PriVal(prival) => {
                    new_args.push(prival);
                },
                _ => {
                    let id = BaId {
                        name: gensym_rand(),
                        splid: None,
                        ty: decval.get_batype(),
                        loc: decval.get_loc()
                    };

                    let dec = BaDeclare {
                        name: id.clone(),
                        value: decval
                    };

                    new_decs.push(dec);

                    new_args.push(BaPriVal::Id(id));
                }
            }

        }

        let new_funcall = BaFunCall {
            name: lspfuncall.name.clone(),
            args: new_args
        };

        let decval = BaDecVal::FunCall(new_funcall);

        (decval, new_decs)
    }

    fn simplify_compexpr(&self, pritn: &LspCompPriTN) -> (BaPriVal, Vec<BaDeclare>) {
        let mut gensym_ser = gen_gensym_ser("tmp", gen_counter());

        self.unfold_compexpr(pritn, &mut gensym_ser)
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Helper

    fn unfold_compexpr(&self, pritn: &LspCompPriTN, gensym_ser: &mut SymGen)
    -> (BaPriVal, Vec<BaDeclare>)
    {
        match pritn {
            LspCompPriTN::Tree(tree_rc) => {
                let tree_ref = tree_rc.as_ref().borrow();

                let (lf_pri, mut lf_decs)
                = self.unfold_compexpr(&tree_ref.lf, gensym_ser);

                let (rh_pri, rh_decs)
                = self.unfold_compexpr(&tree_ref.rh, gensym_ser);

                /* build declare expression */
                let loc = lf_pri.get_loc();
                let decval = BaDecVal::TwoAddr(
                    tree_ref.bop.clone(),
                    lf_pri,
                    rh_pri,
                );
                let decid = BaId {
                    name: gensym_ser(),
                    splid: None,
                    ty: Some(decval.to_type()),
                    loc
                };

                let dec = BaDeclare {
                    name: decid.clone(),
                    value: decval
                };

                lf_decs.extend(rh_decs.into_iter());
                lf_decs.push(dec);

                (BaPriVal::Id(decid), lf_decs)
            },
            LspCompPriTN::Leaf(lsppri_rc) => {
                let (decval, mut new_decs)
                = self.simplify_pri(&lsppri_rc.as_ref().borrow());

                let newprival = self.decval2prival(decval, &mut new_decs);

                (newprival, new_decs)
            }
        }
    }

    fn decval2prival(&self, decval: BaDecVal, new_decs: &mut Vec<BaDeclare>) -> BaPriVal {
        match decval {
            BaDecVal::PriVal(prival) => {
                prival
            },
            _ => {
                let id = BaId {
                    name: gensym_rand(),
                    splid: None,
                    ty: decval.get_batype(),
                    loc: decval.get_loc()
                };

                let dec = BaDeclare {
                    name: id.clone(),
                    value: decval
                };

                new_decs.push(dec);

                BaPriVal::Id(id)
            }
        }
    }

    fn decval2stmt(&self, decval: BaDecVal) -> Option<BaStmt> {
        match decval {
            BaDecVal::FunCall(funcall) => {
                Some(BaStmt::FunCall(funcall))
            },
            _ => None
        }
    }
}


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
