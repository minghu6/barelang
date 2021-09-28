#![allow(unused_imports)]

use std::borrow::{Borrow, BorrowMut};
use std::rc::Rc;
use std::cell::{
    RefCell, Ref, RefMut
};

use m6stack::{ stack, Stack };
use uuid::Uuid;
use itertools::Itertools;
use indexmap::{IndexMap, indexmap};

use crate::datalsp::*;
use crate::datair::*;
use crate::error::*;
use crate::rsclib::search_rs_lib;
use crate::utils::{
    gen_counter, CounterType
};



/// Function Item is regarded as primitives, only allow to defined on Toplevel
fn collect_fn_item(ml: &ModuleLisp) -> Rc<IndexMap<BaFunKey, SymItem>> {
    let mut funtbl = indexmap! {};

    match ml {
        ModuleLisp::BlockStmts(block_stmts) => {
            for blkstmtsref in block_stmts.block_stmts.iter() {
                match &*blkstmtsref.block_stmt_ref() {
                    LspBlockStmt::Item(lspitem) => {
                        match lspitem {
                            LspItem::DefFun(defn) => {
                                let symitem = SymItem { ty: defn.hdr.ret.clone() };
                                let funkey = defn.hdr.as_key();

                                if funtbl.contains_key(&funkey) {
                                    push_err(
                                        defn.get_loc(),
                                        TrapCode::DuplicatedDefn(&defn.hdr.name).emit_box_err()
                                    )
                                }
                                else {
                                    funtbl.insert(funkey, symitem);
                                }
                            }
                        }
                    },
                    _ => {}
                }
            }
        }
    }

    Rc::new(funtbl)
}

pub struct MLSimplifier {
    funtbl: Rc<IndexMap<BaFunKey, SymItem>>,
    ml: Rc<ModuleLisp>,
    scope_stack: Stack<Rc<RefCell<BaScope>>>,
    scope_counter: CounterType
}

impl MLSimplifier {
    pub fn new(ml: ModuleLisp) -> Self {
        let funtbl = collect_fn_item(&ml);

        Self {
            funtbl,
            ml: Rc::new(ml),
            scope_stack: stack![],
            scope_counter: gen_counter()
        }
    }

    fn push_scope(&mut self) {
        let mut child_scope = BaScope::new((self.scope_counter)());
        child_scope.parent = self.scope_stack.peek().cloned();
        let scope_rc = Rc::new(RefCell::new(child_scope));

        self.scope_stack.push(scope_rc.clone());
    }

    fn pop_scope(&mut self) -> Option<Rc<RefCell<BaScope>>> {
        self.scope_stack.pop()
    }

    fn peek_scope(&self) -> Option<Rc<RefCell<BaScope>>> {
        self.scope_stack.peek().cloned()
    }

    fn current_scope_mut(&self) -> RefMut<BaScope> {
        let current_scope_rc = self.scope_stack.peek().unwrap();

        current_scope_rc.as_ref().borrow_mut()
    }

    fn current_scope(&self) -> Ref<BaScope> {
        let current_scope_rc = self.scope_stack.peek().unwrap();

        current_scope_rc.as_ref().borrow()
    }

    fn insert_sym_item(&mut self, key: String, sym_item: SymItem) -> Option<SymItem> {
        let mut current_scope_ref = self.current_scope_mut();

        current_scope_ref.symtbl.insert(key, sym_item)
    }

    fn get_sym_item(&self, key: &str) -> Option<SymItem> {
        let current_scope = self.current_scope();

        current_scope.get_sym_item(key)
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Entry point

    pub fn simplify(mut self) -> BaMod {

        let mut stmts = vec![];
        let ml = self.ml.clone();

        let bin = match ml.as_ref() {
            ModuleLisp::BlockStmts(block_stmts) => {
                self.push_scope();

                for blkstmtsref in block_stmts.block_stmts.iter() {
                    stmts.extend(
                        self.simplify_blkstmt(&blkstmtsref.block_stmt_ref())
                    );
                }

                let tailval = if let Some(ref lsptailexpr) = block_stmts.tail_expr {
                    let (decval, mut new_decs)
                    = self.simplify_expr(lsptailexpr);

                    let prival = self.decval2prival(decval, &mut new_decs);

                    stmts.extend(new_decs.into_iter().map(|dec| BaStmt::Declare(dec)));

                    Some(prival)
                }
                else {
                    None
                };

                let root_rc = self.pop_scope().unwrap();
                let mut root_ref = root_rc.as_ref().borrow_mut();

                root_ref.stmts = stmts;
                root_ref.tailval = tailval;
                root_ref.parent = None;

                BaMod {
                    scope: root_rc.clone(),
                    funtbl: self.funtbl.clone()
                }
            }
        };

        if err_occured() {
            dump_errs();
            panic!()
        }

        bin
    }

    fn simplify_blkstmt(&mut self, blkstmtsref: &LspBlockStmt) -> Vec<BaStmt> {
        match blkstmtsref {
            LspBlockStmt::Stmt(lspstmt) => {
                self.simplify_stmt(lspstmt)
            },
            LspBlockStmt::Item(item) => {
                match item {
                    LspItem::DefFun(lspdefn) => {
                        vec![BaStmt::DefFun(self.simplify_defn(lspdefn))]
                    }
                }
            },
            LspBlockStmt::Block(block) => {
                vec![BaStmt::Block(self.simplify_block(block))]
            }
        }
    }

    fn simplify_defn(&mut self, lspdefn: &LspDefFun) -> BaDefFun {
        self.push_scope();

        self.setup_scope_symtbl(lspdefn.hdr.params
            .iter()
            .map(|param| -> (String, SymItem) {
                 (param.formal.to_owned(), SymItem { ty: param.ty.to_owned() })
            })
            .collect_vec()
        );

        let body = self._simplify_block_0(&lspdefn.body);

        /* Customize fn block simplification */


        BaDefFun {
            hdr: lspdefn.hdr.clone(),
            body
        }
    }

    // supply symtbl content to current scope
    fn setup_scope_symtbl(&mut self, income_symtbl: Vec<(String, SymItem)>) {
        let scope_rc = self.peek_scope().unwrap();
        let mut scope_ref = scope_rc
        .as_ref()
        .borrow_mut();

        scope_ref.symtbl.extend(income_symtbl.into_iter());
    }

    fn simplify_block(&mut self, lspblock: &LspBlock) -> Rc<RefCell<BaScope>> {
        self.push_scope();

        self._simplify_block_0(lspblock)
    }

    fn _simplify_block_0(&mut self, lspblock: &LspBlock) -> Rc<RefCell<BaScope>> {

        let mut stmts = vec![];

        for blkstmtsref in lspblock.block_stmts.block_stmts.iter() {
            stmts.extend(
                self.simplify_blkstmt(&blkstmtsref.block_stmt_ref())
            );
        }

        let tailval = if let Some(ref lspexpr) = lspblock.block_stmts.tail_expr {
            let (decval, mut new_decs) = self.simplify_expr(&lspexpr);

            let prival = self.decval2prival(decval, &mut new_decs);

            stmts.extend(new_decs.into_iter().map(|dec| BaStmt::Declare(dec)));

            Some(prival)
        }
        else {
            None
        };

        let scope = self.pop_scope().unwrap();
        let mut scope_ref = scope
        .as_ref()
        .borrow_mut();

        scope_ref.stmts = stmts;
        scope_ref.tailval = tailval;

        scope.clone()
    }

    fn simplify_stmt(&mut self, lspstmt: &LspStmt) -> Vec<BaStmt> {
        match lspstmt {
            LspStmt::Expr(lspexpr) => {
                let (decval, new_decs) = self.simplify_expr(lspexpr);
                self.combine_decval_decs_into_stmts(decval, new_decs)
            },
            LspStmt::Empty => {
                vec![]
            },
            LspStmt::Declare(lsp_dec) => {
                let (decval, new_decs)
                = self.simplify_declare(lsp_dec.as_ref());

                self.combine_decval_decs_into_stmts(decval, new_decs)
            }
        }
    }

    fn simplify_declare(&mut self, lspdec: &LspDeclare) -> (BaDecVal, Vec<BaDeclare>) {
        let lspid = &lspdec.id;
        let lspexpr = &lspdec.val;

        let (decval, mut new_decs)
        = self.simplify_expr(lspexpr);

        self.insert_sym_item(
            lspid.sym(),
            SymItem { ty: decval.get_batype().unwrap() }
        );

        let ty = decval.get_batype();
        let loc = decval.get_loc();
        let prival = self.decval2prival(decval, &mut new_decs);
        let decval = BaDecVal::PriVal(prival);

        new_decs.push(BaDeclare {
            name: BaId {
                name: lspid.name.clone(),
                splid: lspid.splid.clone(),
                ty,
                loc
            },
            value: decval.clone()
        });

        (decval, new_decs)
    }

    fn simplify_expr(&mut self, lspexpr: &LspExpr) -> (BaDecVal, Vec<BaDeclare>) {
        match lspexpr {
            LspExpr::Pri(pri) => {
                self.simplify_pri(pri)
            },
            LspExpr::TwoPri(bop, expr1st, expr2nd) => {
                let (expr1_decval, mut expr1_decs)
                = self.simplify_expr(expr1st);

                let (expr2_decval, mut expr2_decs)
                = self.simplify_expr(expr2nd);

                let ty = expr1_decval.get_batype();
                let loc = expr1_decval.get_loc();

                let fstpri
                = self.decval2prival(expr1_decval, &mut expr1_decs);

                let sndpri
                = self.decval2prival(expr2_decval, &mut expr2_decs);

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

                let mut combined_decs = Vec::new();
                combined_decs.extend(expr1_decs.into_iter());
                combined_decs.extend(expr2_decs.into_iter());
                combined_decs.push(dec);

                (BaDecVal::PriVal(BaPriVal::Id(decid)), combined_decs)
            },
            LspExpr::FunCall(lspfuncall) => {
                self.simplify_funcall(lspfuncall)
            }
        }
    }

    fn simplify_pri(&mut self, lsppri: &LspPri) -> (BaDecVal, Vec<BaDeclare>) {
        match lsppri {
            LspPri::Expr(lspexpr_rc) => {
                self.simplify_expr(&lspexpr_rc.as_ref())
            },
            LspPri::Id(lspid_rc) => {
                let id = self.simplify_id(&lspid_rc.as_ref());

                (BaDecVal::PriVal(BaPriVal::Id(id)), vec![])
            },
            LspPri::Lit(lit) => {
                (BaDecVal::PriVal(BaPriVal::Lit(lit.clone())), vec![])
            }
        }
    }

    fn simplify_id(&self, lspid: &BaId) -> BaId {
        let ty = if let Some(symitem) = self.get_sym_item(&lspid.sym()) {
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

    fn simplify_funcall(&mut self, lspfuncall: &LspFunCall)
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

        let funkey = BaFunKey::from((&lspfuncall.name.name, &new_args));

        let ret = if let Some(ret) = self.funtbl.get(&funkey) {
            ret.ty.clone()
        }
        else if let Some(ref ex_fun_proto)
        = search_rs_lib(&lspfuncall.name()) {
            ex_fun_proto.ret.as_type()
        }
        else {
            push_err(
                lspfuncall.get_loc(),
                TrapCode::UnresolvedFn(&lspfuncall.name).emit_box_err()
            );
            BaType::VoidUnit
        };

        let new_funcall = BaFunCall {
            name: lspfuncall.name.clone(),
            args: new_args,
            ret
        };

        let decval = BaDecVal::FunCall(new_funcall);

        (decval, new_decs)
    }


    ////////////////////////////////////////////////////////////////////////////////
    //// Helper

    fn combine_decval_decs_into_stmts(
        &self, decval: BaDecVal, new_decs: Vec<BaDeclare>
    ) -> Vec<BaStmt>
    {
        let mut stmts = new_decs
        .into_iter()
        .map_into::<BaStmt>()
        .collect_vec();

        if let Some(decstmt) = self.decval2stmt(decval) {
            stmts.push(decstmt);
        }

        stmts
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
//// Structure Enhancement

impl From<(&String, &Vec<BaPriVal>)> for BaFunKey {
    fn from(input: (&String, &Vec<BaPriVal>)) -> Self {
        Self (
            input.0.to_owned(),
            input.1
            .iter()
            .map(|baprval| baprval.get_batype().unwrap())
            .collect_vec()
        )
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
    use std::path::PathBuf;

    use crate::lexer::{tokenize, trim_tokens, SrcFileInfo};
    use crate::manual_parser::Parser;
    use crate::{VerboseLv, VERBOSE};

    use super::MLSimplifier;

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

    #[test]
    fn test_ml_simplifier() {
        let file = SrcFileInfo::new(PathBuf::from("./examples/exp1.ba")).unwrap();

        let tokens = tokenize(&file);
        let tokens = trim_tokens(tokens);

        let mut parser = Parser::new(tokens);
        let ml = parser.parse().unwrap();
        println!("ML:\n{:#?}", ml);

        let mlslf = MLSimplifier::new(ml);
        let bin = mlslf.simplify();
        println!("BaBin:\n{:#?}", bin.scope);
    }
}
