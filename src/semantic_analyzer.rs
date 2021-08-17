//! Semantic Analyzer: translates synax tree into annotation tree manually.

use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    baredata::*,
    syntax_parser::{ASTNode, AST},
};

////////////////////////////////////////////////////////////////////////////////
//// Bare Lang Node


pub fn analyze_semantic(ast: Rc<RefCell<AST>>) -> AT {
    analyze_ast(&ast)
}


pub fn analyze_ast(ast: &Rc<RefCell<AST>>) -> AT {
    let ast_ref = ast.as_ref().borrow();
    let (fstsym, fstnode) = ast_ref.elems_vec().into_iter().next().unwrap();

    match fstsym.to_string().as_str() {
        "[BlockStmts]" => AT::BlockStmts(analyze_block_stmts(fstnode)),
        //"Block" => analyze_block(ast_node),
        _ => {
            unreachable!()
        }
    }
}

pub fn analyze_block_stmts(ast_node: &ASTNode) -> Vec<BaBlockStmt> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut stmts = vec![];

    stmts.push(
        analyze_block_stmt(fstelem)
    );

    if let Some(sndres) = elems_iter.next() {
        // recur
        stmts.extend(analyze_block_stmts(sndres.1))
    }

    stmts
}

pub fn analyze_block_stmt(ast_node: &ASTNode) -> BaBlockStmt {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    BaBlockStmt::Stmt(analyze_stmt(fstelem))
}

pub fn analyze_block(ast_node: &ASTNode) {
    let _ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
}

pub fn analyze_stmt(ast_node: &ASTNode) -> BaStmt {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    BaStmt::Expr(analyze_expr(fstelem))
}

pub fn analyze_expr(ast_node: &ASTNode) -> BaExpr {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Pri]" => {
            let pri = analyze_pri(fstelem);
            BaExpr::Pri(pri)
            // ignore parse Expr1
        },
        "[FunCall]" => BaExpr::FunCall(
            analyze_fun_call(fstelem)
        ),
        "[VariableDeclarator]" => BaExpr::Pri(
            BaPri::Id(analyze_variable_declarator(fstelem))
        ),
        _ => unreachable!(),
    }
}

pub fn analyze_pri(ast_node: &ASTNode) -> BaPri {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "[Lit]" => BaPri::Val(analyze_lit(fstelem)),
        "[Id]" => BaPri::Id(analyze_id(fstelem)),
        _ => unreachable!(),
    }
}

pub fn analyze_expr1(_ast_node: &ASTNode) {}

pub fn analyze_fun_call(ast_node: &ASTNode) -> BaFunCall {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();
    let funid = analyze_id(fstelem);


    elems_iter.next().unwrap();  //skip left paren
    let (_third_sym, third_elem) = elems_iter.next().unwrap();
    let expr_list = analyze_expr_list(third_elem);

    BaFunCall {
        name: funid,
        params: expr_list
    }
}

pub fn analyze_variable_declarator(ast_node: &ASTNode) -> BaId {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();
    elems_iter.next().unwrap();
    let (_third_sym, third_elem) = elems_iter.next().unwrap();

    let name = analyze_t_id(fstelem);
    let expr = analyze_variable_initializer(third_elem);

    BaId {
        name,
        value: BaVal::Expr(Rc::new(expr)),
        splid: None
    }
}

pub fn analyze_variable_initializer(ast_node: &ASTNode) -> BaExpr {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    analyze_expr(fstelem)
}

pub fn analyze_expr_list(ast_node: &ASTNode) -> Vec<BaExpr> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut exprs = vec![];

    exprs.push(
        analyze_expr(fstelem)
    );

    if let Some(_comma_pair) = elems_iter.next() {
        // recur
        exprs.extend(analyze_expr_list(elems_iter.next().unwrap().1))
    }

    exprs
}

pub fn analyze_id(ast_node: &ASTNode) -> BaId {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "<id>" => BaId {
            splid: None,
            name: analyze_t_id(fstelem),
            value: BaVal::Unresolved
        },
        "<splid>" => {
            let splid = analyze_t_splid(fstelem);
            let t_id = analyze_t_id(elems_iter.next().unwrap().1);
            BaId {
                splid: Some(splid),
                name: t_id,
                value: BaVal::Unresolved
            }
        }
        _ => unreachable!(),
    }
}

pub fn analyze_lit(ast_node: &ASTNode) -> BaVal {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    match fstsym.to_string().as_str() {
        "<intlit>" => analyze_t_intlit(fstelem),
        _ => unreachable!()
    }
}

pub fn analyze_t_id(ast_node: &ASTNode) -> String {
    ast_node.get_token().unwrap().as_ref().value().to_string()
}

pub fn analyze_t_splid(ast_node: &ASTNode) -> BaSplId {
    let tokv = ast_node.get_token().unwrap().as_ref().value();

    match tokv {
        "rs#" => BaSplId::RS,
        _ => unreachable!()
    }
}

pub fn analyze_t_intlit(ast_node: &ASTNode) -> BaVal {
    let mut tokv = ast_node.get_token().unwrap().as_ref().value();

    let is_neg = if tokv.starts_with("-") { true } else { false };

    // Handle Hex Number Literal
    if tokv.contains("0x") {
        if tokv.starts_with("-") {
            tokv = tokv.trim_start_matches("-");
        }
        else if tokv.starts_with("+") {
            tokv = tokv.trim_start_matches("+");
        }

        tokv = tokv.trim_start_matches("0x");

        if is_neg {
            BaVal::Num(BaNum::ISize(
                -isize::from_str_radix(tokv,16).unwrap()
            ))
        }
        else {
            BaVal::Num(BaNum::USize(
                usize::from_str_radix(tokv,16).unwrap()
            ))
        }
    }
    else {
        if tokv.starts_with("-") {
            BaVal::Num(BaNum::ISize(
                tokv.parse::<isize>().unwrap()
            ))
        }
        else {
            BaVal::Num(BaNum::USize(
                tokv.parse::<usize>().unwrap()
            ))
        }
    }
}


#[cfg(test)]
mod test {
    #[test]
    fn test_rust_parse() {
        assert_eq!("-1234".parse::<isize>().unwrap(), -1234);
        assert_eq!(
            -isize::from_str_radix(
                "-0x1234".trim_start_matches("-0x"),
                16
            ).unwrap(),
            -0x1234
        );
        assert_eq!(
            -isize::from_str_radix(
                "-0xabcd".trim_start_matches("-0x"),
                16
            ).unwrap(),
            -0xabcd
        );
        assert_ne!(
            -isize::from_str_radix(
                "-abcd",
                16
            ).unwrap(),
            -0xabcd  // actual result is `0xabcd`, sign has been ignored.
        );
    }

    #[test]
    fn test_semantics_analyze() {
        use std::fs;

        use crate::syntax_parser::{ PARSER, Parser };
        use crate::semantic_analyzer::{analyze_semantic};

        let data0 = fs::read_to_string("./examples/exp0.bare").expect("Unable to read file");

        match (*PARSER).parse(&data0) {
            Ok(res) => {

                println!("{}", res.as_ref().borrow());

                let at = analyze_semantic(res);

                println!("{:#?}", at);

            },
            Err(msg) => {
                eprintln!("{}", msg);
            }
        }

    }
}
