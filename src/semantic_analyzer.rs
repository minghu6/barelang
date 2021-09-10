//! Semantic Analyzer: translates synax tree into annotation tree manually.

use indexmap::{ IndexMap };
use lazy_static::lazy_static;

use std::rc::Rc;
use std::{cell::RefCell};
use std::error::Error;

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

pub fn analyze_semantic(ast: Rc<RefCell<AST>>) -> Result<ModuleLisp, Box<dyn Error>> {
    let ml = analyze_ast(&ast);

    ml
}

////////////////////////////////////////////////////////////////////////////////
//// Analyze Non-Terminal Symbol

fn analyze_ast(ast: &Rc<RefCell<AST>>) -> Result<ModuleLisp, Box<dyn Error>> {
    let ast_ref = ast.as_ref().borrow();
    let (fstsym, fstnode) = ast_ref.elems_vec().into_iter().next().unwrap();

    match fstsym.to_string().as_str() {
        "[BlockStmts]" => {
            let blkstmts = analyze_block_stmts(fstnode)?;
            Ok(ModuleLisp::BlockStmts(blkstmts))
        },
        //"Block" => analyze_block(ast_node),
        _ => {
            unreachable!()
        }
    }
}

fn analyze_block_stmts(ast_node: &ASTNode)
 -> Result<Vec<LspBlockStmtRef>, Box<dyn Error>>
{
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut blkstmts = vec![];

    blkstmts.push(LspBlockStmtRef::new(analyze_block_stmt(fstelem)?));

    if let Some(sndres) = elems_iter.next() {
        // recur
        blkstmts.extend(analyze_block_stmts(&sndres.1)?)
    }

    Ok(blkstmts)
}

fn analyze_block_stmt(ast_node: &ASTNode) -> Result<LspBlockStmt, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "[Stmt]" => LspBlockStmt::Stmt(analyze_stmt(fstelem)?),
        "<f>" => {
            todo!()
        },
        _ => unreachable!(),
    })
}

#[allow(unused)]
fn analyze_block(ast_node: &ASTNode) {
    let _ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
}

fn analyze_stmt(ast_node: &ASTNode) -> Result<LspStmt, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "[Expr]" => LspStmt::Expr(analyze_expr(fstelem)?),
        "<semi>" => LspStmt::Empty,
        _ => unreachable!(),
    })
}

fn analyze_expr(ast_node: &ASTNode) -> Result<LspExpr, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "[Pri]" => {
            let fstpri = analyze_pri(fstelem)?;

            // Binary Operator Precedence Parsing
            if let Some((_sndsym, sndelem)) = elems_iter.next() {
                // Expr1
                let expr1_ref = sndelem.get_ast().unwrap().as_ref().borrow();
                let expr1_elems_vec = expr1_ref.elems_vec();
                let mut expr1_elems_iter = expr1_elems_vec.into_iter();
                let (expr1_fstsym, _expr1_fstnode) = expr1_elems_iter.next().unwrap();
                let (_expr1_snd_sym, expr1_snd_node) = expr1_elems_iter.next().unwrap();

                match expr1_fstsym.to_string().as_str() {
                    "[BOp]" => {  // Compact BOp
                        let (mut out_bop_stack, mut out_pri_stack)
                        = analyze_bop_expr1(sndelem)?;

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

                                comppritn_stack.push(
                                LspCompPriTN::Tree(Rc::new(RefCell::new(
                                        LspCompPriT {
                                            bop,
                                            lf: lfnode,
                                            rh: rhnode,
                                        }
                                ))));
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
                    },
                    "<lparen>" => {  // FunCall
                        let funid_rc
                        = fstpri.to_lspid().unwrap();
                        let funid = funid_rc.as_ref().borrow().clone();

                        let expr_list
                        = analyze_expr_list(expr1_snd_node)?;

                        LspExpr::FunCall(LspFunCall {
                            name: funid.as_baid(),
                            args: expr_list,
                        })
                    },
                    "<eq>" => {  // Declare
                        let lspid_rc =
                        if let LspPri::Id(id) = fstpri { id }
                        else { unreachable!() };


                        let expr
                        = analyze_expr(expr1_snd_node)?;

                        let decid = LspId {
                            name: lspid_rc.as_ref().borrow().sym(),
                            splid: None,
                            loc: lspid_rc.as_ref().borrow().loc.clone()
                        };

                        LspExpr::Declare(Rc::new(LspDeclare {
                            id: decid,
                            val: expr
                        }))
                    },
                    _ => unreachable!()
                }

            } else {
                LspExpr::Pri(fstpri)
            }
        }
        _ => unreachable!(),
    })
}

fn analyze_pri(ast_node: &ASTNode) -> Result<LspPri, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "[Lit]" => LspPri::Lit(analyze_lit(fstelem)?),
        "[Id]" => LspPri::Id(Rc::new(RefCell::new(analyze_id(fstelem)?))),
        "<paren>" => LspPri::Expr(Rc::new(RefCell::new(analyze_expr(
            &elems_iter.next().unwrap().1,
        )?))),
        _ => unreachable!(),
    })
}

/// Output: Production Queue
fn analyze_bop_expr1(ast_node: &ASTNode)
 -> Result<(Stack<BaBOp>, Stack<LspPri>), Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let bop
    = analyze_bop(&elems_iter.next().unwrap().1)?;

    let pri
    = analyze_pri(&elems_iter.next().unwrap().1)?;

    Ok(if let Some((_thirdsym, thirdelem)) = elems_iter.next() {
        let (mut bopstack, mut pristack)
        = analyze_bop_expr1(thirdelem)?;

        bopstack.push(bop);
        pristack.push(pri);

        (bopstack, pristack)
    } else {
        (stack![bop], stack![pri])
    })
}

fn analyze_bop(ast_node: &ASTNode) -> Result<BaBOp, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let tok = fstelem.get_token().unwrap();

    Ok(BaBOp::from(tok.as_ref()))
}


fn analyze_expr_list(ast_node: &ASTNode) -> Result<Vec<LspExpr>, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();
    let mut elems_iter = ast_ref.elems_vec().into_iter();
    let (_fstsym, fstelem) = elems_iter.next().unwrap();

    let mut exprs = vec![];

    exprs.push(analyze_expr(fstelem)?);

    if let Some(_comma_pair) = elems_iter.next() {
        // recur
        let rem_expr_list
        = analyze_expr_list(&elems_iter.next().unwrap().1)?;

        exprs.extend(rem_expr_list);
    }

    Ok(exprs)
}

fn analyze_id(ast_node: &ASTNode) -> Result<LspId, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "<id>" => analyze_t_id(fstelem)?,
        "<splid>" => {
            let splid = analyze_t_splid(fstelem)?;
            let mut t_id
            = analyze_t_id(&elems_iter.next().unwrap().1)?;

            t_id.splid = Some(splid);

            t_id
        },
        _ => unreachable!("{}", fstsym),
    })
}

fn analyze_lit(ast_node: &ASTNode) -> Result<BaLit, Box<dyn Error>> {
    let ast_ref = ast_node.get_ast().unwrap().as_ref().borrow();

    let mut elems_iter = ast_ref.elems_vec().into_iter();

    let (fstsym, fstelem) = elems_iter.next().unwrap();

    Ok(match fstsym.to_string().as_str() {
        "<intlit>" => analyze_t_intlit(fstelem)?,
        "<dqstr>" => analyze_t_dqstr(fstelem)?,
        _ => unreachable!(),
    })
}

////////////////////////////////////////////////////////////////////////////////
//// Analyze Terminal Symbol

fn analyze_t_id(ast_node: &ASTNode) -> Result<LspId, Box<dyn Error>> {
    let tok = ast_node.get_token().unwrap().as_ref();

    Ok(LspId {
        name: tok.value().to_string(),
        splid: None,
        loc: tok.loc()
    })
}

fn analyze_t_splid(ast_node: &ASTNode) -> Result<BaSplId, Box<dyn Error>> {
    let tokv = ast_node.get_token().unwrap().as_ref().value();

    Ok(match tokv {
        "rs#" => BaSplId::RS,
        _ => unreachable!(),
    })
}

fn analyze_t_dqstr(ast_node: &ASTNode) -> Result<BaLit, Box<dyn Error>> {
    let tok = ast_node.get_token().unwrap().as_ref();

    let strv = String::from(tok.value());

    Ok(BaLit::Str(BaStr {
        val: strv,
        loc: tok.loc()
    }))
}

fn analyze_t_intlit(ast_node: &ASTNode) -> Result<BaLit, Box<dyn Error>> {
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

    Ok(BaLit::I32(BaI32 {
        val: bai32val,
        loc: tok.loc()
    }))
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

        use crate::semantic_analyzer::{analyze_ast};

        use std::path::PathBuf;
        use crate::lexer::{
            SrcFileInfo
        };
        use crate::rules::{
            barelang_gram
        };
        use crate::syntax_parser::{
            LL1Parser, Parser
        };


        let srcfile
        = SrcFileInfo::new(PathBuf::from("./examples/exp0.ba")).unwrap();

        let gram = barelang_gram();
        let parser = LL1Parser::new(gram);

        match parser.parse(&srcfile) {
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
