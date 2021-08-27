//! Semantic Analyzer: translates synax tree into annotation tree manually.

use indexmap::{ IndexMap };
use lazy_static::lazy_static;

use std::rc::Rc;
use std::{cell::RefCell};

use crate::{
    datalsp::*,
    datair::*,
    rules::bopprecmap,
    stack,
    syntax_parser::{ASTNode, AST},
    utils::{ Stack },
};

lazy_static! {
    pub static ref BOP_PREC_MAP: IndexMap<BaBOp, usize> = bopprecmap();
}

pub fn analyze_semantic(ast: Rc<RefCell<AST>>) -> ModuleLisp {
    let ml = analyze_ast(&ast);

    ml
}

////////////////////////////////////////////////////////////////////////////////
//// Bare Lang semantics analyze step 0:- Create Annotated Tree

fn analyze_ast(ast: &Rc<RefCell<AST>>) -> ModuleLisp {
    let ast_ref = ast.as_ref().borrow();
    let (fstsym, fstnode) = ast_ref.elems_vec().into_iter().next().unwrap();

    match fstsym.to_string().as_str() {
        "[BlockStmts]" => ModuleLisp::BlockStmts(analyze_block_stmts(fstnode)),
        //"Block" => analyze_block(ast_node),
        _ => {
            unreachable!()
        }
    }
}

fn analyze_block_stmts(ast_node: &ASTNode) -> Vec<LspBlockStmtRef> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut stmts = vec![];

    stmts.push(LspBlockStmtRef::new(analyze_block_stmt(fstelem)));

    if let Some(sndres) = elems_iter.next() {
        // recur
        stmts.extend(analyze_block_stmts(&sndres.1))
    }

    stmts
}

fn analyze_block_stmt(ast_node: &ASTNode) -> LspBlockStmt {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Stmt]" => LspBlockStmt::Stmt(analyze_stmt(fstelem)),
        "[VariableDeclarator]" => {
            let (id, expr) = analyze_variable_declarator(fstelem);
            LspBlockStmt::Declare(id, expr)
        },
        _ => unreachable!(),
    }
}

#[allow(unused)]
fn analyze_block(ast_node: &ASTNode) {
    let _ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
}

fn analyze_stmt(ast_node: &ASTNode) -> LspStmt {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Expr]" => LspStmt::Expr(analyze_expr(fstelem)),
        "<semi>" => LspStmt::Empty,
        _ => unreachable!(),
    }
}

fn analyze_expr(ast_node: &ASTNode) -> LspExpr {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Pri]" => {
            let fstpri = analyze_pri(fstelem);

            // Binary Operator Precedence Parsing
            if let Some((_sndsym, sndelem)) = elems_iter.next() {
                let (mut out_bop_stack, mut out_pri_stack) = analyze_expr1(sndelem);

                let mut staging_bop_stack: Stack<BaBOp> = stack![];
                let mut comppritn_stack
                = stack![LspCompPriTN::Leaf(Rc::new(RefCell::new(fstpri)))];

                // out_bop_stack would be same with out_pri_stack in size.
                while !(out_bop_stack.is_empty() && staging_bop_stack.is_empty()) {
                    // Reduce
                    if out_bop_stack.is_empty()
                        || !staging_bop_stack.is_empty()
                            && staging_bop_stack.peek().unwrap().precedence()
                                // >= for left associative operator
                                >= out_bop_stack.peek().unwrap().precedence()
                    {
                        let bop = staging_bop_stack.pop().unwrap();
                        let rhnode = comppritn_stack.pop().unwrap();
                        let lfnode = comppritn_stack.pop().unwrap();

                        comppritn_stack.push(LspCompPriTN::Tree(Rc::new(RefCell::new(LspCompPriT {
                            bop,
                            lf: lfnode,
                            rh: rhnode,
                        }))));
                    }
                    // Shift
                    else {
                        staging_bop_stack.push(out_bop_stack.pop().unwrap());
                        comppritn_stack.push(LspCompPriTN::Leaf(
                            Rc::new(RefCell::new(out_pri_stack.pop().unwrap()))
                        ));
                    }
                }

                LspExpr::CompPri(comppritn_stack.pop().unwrap())
            } else {
                LspExpr::Pri(fstpri)
            }
        }
        "[FunCall]" => LspExpr::FunCall(analyze_fun_call(fstelem)),
        _ => unreachable!(),
    }
}

fn analyze_pri(ast_node: &ASTNode) -> LspPri {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Lit]" => LspPri::Lit(analyze_lit(fstelem)),
        "[Id]" => LspPri::Id(Rc::new(RefCell::new(analyze_id(fstelem)))),
        "<paren>" => LspPri::Expr(Rc::new(RefCell::new(analyze_expr(
            &elems_iter.next().unwrap().1,
        )))),
        _ => unreachable!(),
    }
}

/// Output: Production Queue
fn analyze_expr1(ast_node: &ASTNode) -> (Stack<BaBOp>, Stack<LspPri>) {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let bop = analyze_bop(&elems_iter.next().unwrap().1);
    let pri = analyze_pri(&elems_iter.next().unwrap().1);

    if let Some((_thirdsym, thirdelem)) = elems_iter.next() {
        let (mut bopstack, mut pristack) = analyze_expr1(thirdelem);
        bopstack.push(bop);
        pristack.push(pri);

        (bopstack, pristack)
    } else {
        (stack![bop], stack![pri])
    }
}

fn analyze_bop(ast_node: &ASTNode) -> BaBOp {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let tok = fstelem.get_token().unwrap();

    BaBOp::from(tok.as_ref())
}

fn analyze_fun_call(ast_node: &ASTNode) -> LspFunCall {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();
    let funid = analyze_id(fstelem);

    elems_iter.next().unwrap(); //skip left paren
    let (_third_sym, third_elem) = elems_iter.next().unwrap();
    let expr_list = analyze_expr_list(third_elem);

    LspFunCall {
        name: funid.as_baid(),
        args: expr_list,
    }
}

fn analyze_variable_declarator(ast_node: &ASTNode) -> (LspId, LspExpr) {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();
    elems_iter.next().unwrap();
    let (_third_sym, third_elem) = elems_iter.next().unwrap();

    let id_lspid = analyze_t_id(fstelem);
    let expr = analyze_variable_initializer(third_elem);


    let decid = LspId {
        name: id_lspid.sym(),
        splid: None,
        loc: id_lspid.loc.clone()
    };

    (decid, expr)
}

fn analyze_variable_initializer(ast_node: &ASTNode) -> LspExpr {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    analyze_expr(fstelem)
}

fn analyze_expr_list(ast_node: &ASTNode) -> Vec<LspExpr> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut exprs = vec![];

    exprs.push(analyze_expr(fstelem));

    if let Some(_comma_pair) = elems_iter.next() {
        // recur
        exprs.extend(analyze_expr_list(&elems_iter.next().unwrap().1))
    }

    exprs
}

fn analyze_id(ast_node: &ASTNode) -> LspId {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "<id>" => analyze_t_id(fstelem),
        "<splid>" => {
            let splid = analyze_t_splid(fstelem);
            let mut t_id = analyze_t_id(&elems_iter.next().unwrap().1);

            t_id.splid = Some(splid);

            t_id
        }
        _ => unreachable!(),
    }
}

fn analyze_lit(ast_node: &ASTNode) -> BaLit {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "<intlit>" => analyze_t_intlit(fstelem),
        _ => unreachable!(),
    }
}

fn analyze_t_id(ast_node: &ASTNode) -> LspId {
    let tok = ast_node.get_token().unwrap().as_ref();

    LspId {
        name: tok.value().to_string(),
        splid: None,
        loc: tok.loc()
    }
}

fn analyze_t_splid(ast_node: &ASTNode) -> BaSplId {
    let tokv = ast_node.get_token().unwrap().as_ref().value();

    match tokv {
        "rs#" => BaSplId::RS,
        _ => unreachable!(),
    }
}

fn analyze_t_intlit(ast_node: &ASTNode) -> BaLit {
    let tok = ast_node.get_token().unwrap().as_ref();
    let mut tokv = tok.value();

    let is_neg = if tokv.starts_with("-") { true } else { false };

    // Handle Hex Number Literal
    let bai32val = if tokv.contains("0x") {
        if tokv.starts_with("-") {
            tokv = tokv.trim_start_matches("-");
        } else if tokv.starts_with("+") {
            tokv = tokv.trim_start_matches("+");
        }

        tokv = tokv.trim_start_matches("0x");

        if is_neg {
            -i32::from_str_radix(tokv, 16).unwrap()
        } else {
            i32::from_str_radix(tokv, 16).unwrap()
        }
    } else {
        tokv.parse::<i32>().unwrap()
    };

    BaLit::I32(BaI32 {
        val: bai32val,
        loc: tok.loc()
    })
}


#[cfg(test)]
mod test {
    #[test]
    fn test_rust_parse() {
        assert_eq!("-1234".parse::<isize>().unwrap(), -1234);
        assert_eq!(
            -isize::from_str_radix("-0x1234".trim_start_matches("-0x"), 16).unwrap(),
            -0x1234
        );
        assert_eq!(
            -isize::from_str_radix("-0xabcd".trim_start_matches("-0x"), 16).unwrap(),
            -0xabcd
        );
        assert_ne!(
            -isize::from_str_radix("-abcd", 16).unwrap(),
            -0xabcd // actual result is `0xabcd`, sign has been ignored.
        );
    }

    #[test]
    fn test_semantics_analyze() {

        use crate::syntax_parser::{Parser, PARSER};
        use crate::semantic_analyzer::{analyze_ast};

        use std::path::PathBuf;
        use crate::lexer::{
            SrcFileInfo
        };


        let srcfile
        = SrcFileInfo::new(PathBuf::from("./examples/exp0.ba")).unwrap();

        match (*PARSER).parse(&srcfile) {
            Ok(res) => {
                println!("{}", res.as_ref().borrow());

                let ml = analyze_ast(&res);

                println!("{:#?}", ml);

            }
            Err(msg) => {
                eprintln!("{}", msg);
            }
        }
    }
}
