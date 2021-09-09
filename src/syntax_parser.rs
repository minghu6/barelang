//! Synax Parser: translates directly into synax tree based on rule.rs.

use itertools::Itertools;
use lazy_static::lazy_static;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

// use crate::*;
use crate::VerboseLv;
use crate::gram::*;
use super::lexer::*;
use crate::rules::barelang_gram;
use super::utils::Stack;
use crate::{
    VERBOSE
};

////////////////////////////////////////////////////////////////////////////////
/////// AST

/// AST Node
#[derive(Debug)]
pub enum ASTNode {
    Tree(Rc<RefCell<AST>>),
    Leaf(Rc<Token>),
}

impl ASTNode {
    pub fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        match self {
            Self::Leaf(token) => writeln!(f, "{}({}){}", padding, padlevel, *token),
            Self::Tree(ast) => {
                let ast_ref = ast.as_ref().borrow();
                ast_ref.dump(f, padlevel)
            }
        }
    }

    pub fn get_token(&self) -> Option<&Rc<Token>> {
        match self {
            Self::Leaf(token) => Some(token),
            _ => None,
        }
    }

    pub fn get_ast(&self) -> Option<&Rc<RefCell<AST>>> {
        match self {
            Self::Tree(ast) => Some(ast),
            _ => None,
        }
    }

    pub fn to_gram_sym(&self) -> GramSym {
        match self {
            Self::Tree(ast) => ast.as_ref().borrow().sym().to_owned(),
            Self::Leaf(token) => token.as_ref().to_gram_sym(),
        }
    }
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Tree(tree) => {
                write!(f, "{}", tree.as_ref().borrow())?;
            },
            Self::Leaf(token) => {
                writeln!(f, "{}", token.as_ref())?;
            }
        }

        Ok(())
    }
}

/// AST
#[derive(Debug)]
pub struct AST {
    /// AST's grammar type
    sym: GramSym,
    elems: Vec<(GramSym, ASTNode)>,
}

impl AST {
    pub fn new(sym: &GramSym) -> Self {
        Self {
            sym: sym.clone(),
            elems: vec![],
        }
    }

    pub fn sym(&self) -> &GramSym {
        &self.sym
    }

    pub fn elem_syms(&self) -> Vec<GramSym> {
        self.elems.iter().map(|x| x.0.clone()).collect_vec()
    }

    pub fn elems_vec(&self) -> Vec<&(GramSym, ASTNode)> {
        self.elems.iter().collect_vec()
    }

    pub fn get_elem(&self, sym: &GramSym) -> Option<&ASTNode> {
        for (each_sym, each_elem) in self.elems.iter() {
            if each_sym == sym { return Some(each_elem) }
        }

        None
    }

    pub fn insert_leaf(&mut self, token: Token) {
        let leaf_name = token.to_gram_sym();
        let leaf = ASTNode::Leaf(Rc::new(token));

        self.elems.push((leaf_name, leaf));
    }

    pub fn insert_tree(&mut self, tree: Rc<RefCell<AST>>) {
        let tree_name = tree.as_ref().borrow().sym().clone();
        let tree = ASTNode::Tree(tree);

        self.elems.push((tree_name, tree));
    }

    pub fn insert_node(&mut self, node: ASTNode) {
        self.elems.push((node.to_gram_sym().to_owned(), node));
    }

    fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        writeln!(f, "{}({}){}: ", padding, padlevel, self.sym())?;

        for (_elem_sym, elem_node) in self.elems.iter() {
            elem_node.dump(f, padlevel + 1)?;
        }

        Ok(())
    }

    /// There are no circle dependency on Tree
    #[allow(unused)]
    fn copy_tree(&self) -> Rc<RefCell<Self>> {
        let mut new_tree = Self::new(self.sym());

        for (sym, node) in self.elems.iter() {
            match node {
                ASTNode::Leaf(token) => {
                    new_tree
                        .elems
                        .push((sym.clone(), ASTNode::Leaf(token.clone())));
                }
                ASTNode::Tree(subtree) => {
                    new_tree.insert_tree(subtree.as_ref().borrow().copy_tree());
                }
            }
        }

        Rc::new(RefCell::new(new_tree))
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f, 0)
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// Parser Trait
pub trait Parser {
    fn parse(&self, source: &SrcFileInfo) -> Result<Rc<RefCell<AST>>, String>;
}


////////////////////////////////////////////////////////////////////////////////
/////// 带回溯的 变形 LL(1) Parser
pub struct LL1Parser {
    name: String,
    gram: Gram,
    prediction_sets: PredSet,
}

type LL1ParseStatesStack = Vec<(Rc<RefCell<AST>>, Stack<GramSym>)>;

impl LL1Parser {
    pub fn new(gram: Gram) -> Self {
        let first_sets = gram.first_sets();
        let follow_sets = gram.follow_sets(&first_sets);
        let prediction_sets = gram.prediction_sets(&first_sets, &follow_sets);

        Self {
            name: gram.name().to_string(),
            gram,
            prediction_sets,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn predict_prod(
        &self,
        lfsym: &GramSym,
        la: PredSetSym,
    ) -> Option<&GramProd>
    {
        self.prediction_sets.predict(lfsym, la)
    }
}

// Impl for Parser
impl Parser for LL1Parser {
    fn parse(&self, source: &SrcFileInfo) -> Result<Rc<RefCell<AST>>, String> {
        let tokens = tokenize(source);

        // trim tokens
        let tokens = tokens.into_iter().filter(|tok| {
            let token_name = tok.name();

            !(token_name == "sp" || token_name.ends_with("comment"))
        }).collect_vec();

        if tokens.is_empty() {
            return Err("empty tokens".to_string());
        }

        if unsafe { VERBOSE == VerboseLv::V2 } {
            println!("tokens: {:#?}\n", tokens);
            println!("LL(1): ");
        }

        let start_sym = self.gram.start_sym().unwrap();
        let root = Rc::new(RefCell::new(AST::new(&start_sym)));

        let mut res = Err(format!(
            "Unexpected token: `{}` for root grammar",
            tokens[0]
        ));

        // Check root， 分支预测
        if let Some(prod)
        = self.predict_prod(start_sym, tokens[0].to_pred_set_sym()) {
            if let GramSymStr::Str(gramsym_vec) = &prod.rhstr {
                // gramsym_vec rev for stack
                let states_stack = vec![(root.clone(), Stack::from(gramsym_vec.clone()))];

                match ll1_parse(&self, &tokens[..], states_stack) {
                    Ok(_res) => {
                        res = Ok(_res);
                    }
                    Err(msg) => {
                        res = Err(msg);
                    }
                }

            }
            else {
                unreachable!()
            }
        }
        else {
            println!("token0: {}", tokens[0].to_pred_set_sym());
            println!("{}", self.prediction_sets);
            unreachable!()
        }

        res
    }
}


impl fmt::Display for Stack<GramSym> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() { return Ok(()) }

        let lastpos = self.len() - 1;
        for (i, item) in self.stack_iter().enumerate() {
            if i < lastpos {
                write!(f, "{},", item)?
            } else {
                write!(f, "{}", item)?
            }
        }

        Ok(())
    }
}


/// Result: <ASTRoot, UnsupportedTokenType>
fn ll1_parse(
    parser: &LL1Parser,
    tokens: &[Token],
    mut states_stack: LL1ParseStatesStack,
) -> Result<Rc<RefCell<AST>>, String> {
    if tokens.is_empty() {
        return Err("empty tokens".to_string());
    }

    let root = states_stack[0].0.clone();
    let tokenslen = tokens.len();
    let tokenslastpos = tokenslen - 1;
    let mut i = 0;

    while let Some((cur_ast, mut symstr_stack)) = states_stack.pop() {
        if unsafe { VERBOSE == VerboseLv::V2 } {
            println!(
                ">>> `{} => ...{}`",
                cur_ast.as_ref().borrow().sym(),
                symstr_stack
            );
        }

        // 分支匹配，遇到终结符直接匹配，遇到非终结符就入栈回到起点
        while let Some(right_sym) = symstr_stack.pop() {
            if i > tokenslastpos {
                if let Some(_)  // 检查当前产生式是否允许结束
                = parser.predict_prod(&right_sym, PredSetSym::EndMarker)
                {
                    return Ok(root);
                } else {
                    return Err(format!(
                        "Unfinished production: {:?}",
                        (
                            cur_ast.as_ref().borrow().sym(),
                            symstr_stack
                        )
                    ));
                }
            }

            if right_sym.is_terminal() {
                if unsafe { VERBOSE == VerboseLv::V2 } {
                    println!("? eat terminal: `{}`", right_sym);
                }

                if right_sym == tokens[i].to_gram_sym() {
                    cur_ast.as_ref().borrow_mut().insert_leaf(tokens[i].clone());

                    // cosume a token
                    if unsafe { VERBOSE == VerboseLv::V2 } {
                        println!("! eaten token: {:?}", tokens[i]);
                    }

                    i += 1;

                    if i == tokenslen {
                        break;
                    }
                }
                else {
                    return Err(format!(
                        "Unmatched token{}, a {} expected",
                        tokens[i], right_sym
                    ));
                }
            }
            else { // handle nonterminal

                if let Some(prod)
                = parser.predict_prod(&right_sym, tokens[i].to_pred_set_sym()) {

                    match &prod.rhstr {
                        GramSymStr::Str(symstr_vec) => {
                            // 保存环境， 入栈
                            let sub_sym_tree = Rc::new(RefCell::new(AST::new(&right_sym)));
                            cur_ast
                            .as_ref()
                            .borrow_mut()
                            .insert_tree(sub_sym_tree.clone());
                            states_stack.push((cur_ast.clone(), symstr_stack.clone()));

                            // 在计算predsets时已经把epsilon str的情况单独提出来了
                            states_stack.push((sub_sym_tree, Stack::from(symstr_vec.clone())));

                            if unsafe { VERBOSE == VerboseLv::V2 } {
                                println!(
                                    "  -> `{}`: `{}`",
                                    right_sym,
                                    GramSymStr::Str(symstr_vec.clone())
                                );
                            }

                            break;
                        },
                        GramSymStr::Epsilon => {
                            continue;
                        }
                    }
                }
                else {

                }
            }
        } // end while rhsymstr

        if unsafe { VERBOSE == VerboseLv::V2 } {
            println!();
        }
    } // end while lfsym

    if i < tokenslastpos {
        return Err(format!(
            "Tokens remains: `{:?}`",
            tokens.get(i..tokenslen).unwrap()
        ));
    }

    Ok(root)
}


lazy_static! {
    pub static ref PARSER: LL1Parser = LL1Parser::new(barelang_gram());
}


#[cfg(test)]
mod test {
    use crate::syntax_parser::Parser;

    #[test]
    fn test_syntax() {
        use std::path::PathBuf;
        use crate::lexer::{
            SrcFileInfo
        };
        use super::PARSER;
        use crate::{
            VERBOSE,
            VerboseLv
        };

        unsafe { VERBOSE = VerboseLv::V2 }

        let srcfile
        = SrcFileInfo::new(PathBuf::from("./examples/exp0.ba")).unwrap();

        match (*PARSER).parse(&srcfile) {
            Ok(res) => {
                println!("{}", res.as_ref().borrow())
            },
            Err(msg) => {
                eprintln!("{}", msg);
            }
        }

    }
}
