//! Synax Parser: translates directly into synax tree based on rule.rs.

use itertools::Itertools;
use lazy_static::lazy_static;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

// use crate::*;
use crate::gram::*;
use super::lexer::*;
use crate::rules::barelang_gram;
use super::utils::Stack;

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
    prediction_sets: PredLL1Sets,
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

    pub fn get_pred_str(
        &self,
        predsym: &PredSetSym,
        leftsym_expcted: &GramSym,
    ) -> Option<Vec<&GramSymStr>> {
        let prods_set;
        match self.prediction_sets.get(predsym) {
            Some(value) => prods_set = value,
            None => {
                return None;
            }
        };

        let matched_symstr_vec = prods_set
            .into_iter()
            .filter(|(leftsym, _symstr)| leftsym == leftsym_expcted)
            .map(|(_leftsym, symstr)| symstr)
            .collect_vec();

        if matched_symstr_vec.is_empty() {
            None
        } else {
            Some(matched_symstr_vec)
        }
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

        println!("tokens: {:#?}\n", tokens);
        println!("LL(1): ");

        let start_sym = self.gram.start_sym().unwrap();
        let root = Rc::new(RefCell::new(AST::new(&start_sym)));

        let mut res = Err(format!(
            "Unexpected token: `{}` for root grammar",
            tokens[0]
        ));

        // Check root， 分支预测
        if let Some(poss_brs) = self.get_pred_str(&tokens[0].to_pred_set_sym(), start_sym) {
            let mut poss_brs_iter = poss_brs.into_iter();

            while let Some(symstr) = poss_brs_iter.next() {
                if let GramSymStr::Str(gramsym_vec) = symstr {
                    // gramsym_vec rev for stack
                    let states_stack = vec![(root.clone(), Stack::from(gramsym_vec.clone()))];

                    if let Ok(_res) = _ll1_parse(&self, &tokens[..], states_stack) {
                        res = Ok(_res);
                        break;
                    }
                }
            }
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
fn _ll1_parse(
    parser: &LL1Parser,
    tokens: &[Token],
    mut states_stack: LL1ParseStatesStack,
) -> Result<Rc<RefCell<AST>>, String> {
    if tokens.is_empty() {
        return Err("empty tokens".to_string());
    }

    if states_stack.is_empty() {
        return Err("states stack should keep filled up by invoker".to_string());
    }

    let root = states_stack[0].0.clone();
    let tokenslen = tokens.len();
    let tokenslastpos = tokenslen - 1;
    let mut i = 0;

    while let Some((cur_ast, mut symstr_stack)) = states_stack.pop() {
        println!(
            ">>> `{} => ...{}`",
            cur_ast.as_ref().borrow().sym(),
            symstr_stack
        );

        // 分支匹配，遇到终结符直接匹配，遇到非终结符就入栈回到起点
        while let Some(right_sym) = symstr_stack.pop() {
            if i > tokenslastpos {
                if let Some(_)  // 检查当前产生式是否允许结束
                = parser.get_pred_str(&PredSetSym::EndMarker, &right_sym)
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
                println!("? eat terminal: `{}`", right_sym);

                if right_sym == tokens[i].to_gram_sym() {
                    cur_ast.as_ref().borrow_mut().insert_leaf(tokens[i].clone());

                    // cosume a token
                    println!("! eaten token: {:?}", tokens[i]);

                    i += 1;
                    // while i < tokenslen && tokens[i].name().ends_with("comment") {
                    //     println!("...skip comment token: {}", tokens[i]);
                    //     i += 1;
                    // }
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
            else {
                // handle nonterminal
                // LL(1)的带回溯的分支预测
                // case-0 (recur epsilon skip)
                // case-1 (just goon)
                // case-2 (push, loop continue)
                // case-3 (return res<ok/error>)
                // case-4 (error)

                // 为方便起见先处理epsilon转换的情况
                if let Some(_) = parser.get_pred_str(&PredSetSym::Epsilon, &right_sym) {
                    println!("?.? try to skip epsilon: `{}` (token: {})", right_sym, tokens[i]);
                    states_stack.push((cur_ast.clone(), symstr_stack.clone()));

                    // 实际上是拷贝了整个树
                    let new_states_stack = _copy_ll1_states_stack(&states_stack);

                    let (_, new_tokens) = tokens.split_at(i);

                    // right_sym 完成匹配
                    if let Ok(_res) = _ll1_parse(parser, new_tokens, new_states_stack) {
                        return Ok(_res);
                    } else {
                        println!("*<x `{}`", right_sym);
                    }

                    // clean env
                    states_stack.pop();
                }

                if let Some(poss_brs) =
                    parser.get_pred_str(&tokens[i].to_pred_set_sym(), &right_sym)
                {
                    let sub_sym_tree = Rc::new(RefCell::new(AST::new(&right_sym)));
                    cur_ast
                    .as_ref()
                    .borrow_mut()
                    .insert_tree(sub_sym_tree.clone());
                    states_stack.push((cur_ast.clone(), symstr_stack.clone()));

                    // case-1
                    // 正经的LL(1) 匹配
                    if poss_brs.len() == 1 {
                        let pred_symstr = poss_brs[0];
                        // 在计算predsets时已经把epsilon str的情况单独提出来了
                        let norm_sym_vec = pred_symstr.get_normal().unwrap();

                        println!(
                            "?>! `{}`: `{}`",
                            right_sym,
                            GramSymStr::Str(norm_sym_vec.clone())
                        );

                        states_stack.push((sub_sym_tree, Stack::from(norm_sym_vec.clone())));

                        break;
                    }

                    // case-2, 需要实现树的复制
                    // 回溯的LL(1) 匹配
                    // else， 前面必定返回，所以不必加额外的else block来消耗宝贵的缩进资源
                    let mut poss_brs_iter = poss_brs.into_iter();

                    while let Some(pred_symstr) = poss_brs_iter.next() {
                        if let GramSymStr::Str(norm_sym_vec) = pred_symstr {
                            println!(
                                "?>? `{}`: `{}`",
                                right_sym,
                                GramSymStr::Str(norm_sym_vec.clone())
                            );

                            states_stack
                                .push((sub_sym_tree.clone(), Stack::from(norm_sym_vec.clone())));

                        }
                        else {  // Epsilon
                            unreachable!("epsillon 在之前已被单独处理")
                        }

                        // 实际上是拷贝了整个树
                        let new_states_stack = _copy_ll1_states_stack(&states_stack);

                        let (_, new_tokens) = tokens.split_at(i);

                        // right_sym 完成匹配
                        if let Ok(_res) = _ll1_parse(parser, new_tokens, new_states_stack) {
                            return Ok(_res);
                        } else {
                            println!("*<x `{}`", right_sym);
                        }

                        // clean env
                        states_stack.pop();
                    }

                    return Err(format!(
                        "All possible branch has been failed for grammar: `{}`",
                        right_sym
                    ));
                }
                // case-3
                // 如果找不到符合条件的分支，就试一下right_sym是否存在epsilon转换
                else if let Some(_) = parser.get_pred_str(&PredSetSym::Epsilon, &right_sym) {
                    println!(" ... skipp epsilon: `{}` (token: {})", right_sym, tokens[i]);
                    // just skip epsilon
                }
                // case-4
                else {
                    return Err(format!(
                        "Unexpected token: `{}` for grammar: `{}`",
                        tokens[i], right_sym
                    ));
                }
            }
        } // end while

        println!();
    }

    if i < tokenslastpos {
        return Err(format!(
            "Tokens remains: `{:?}`",
            tokens.get(i..tokenslen).unwrap()
        ));
    }

    Ok(root)
}

fn _copy_ll1_states_stack(states_stack: &LL1ParseStatesStack) -> LL1ParseStatesStack {
    if states_stack.is_empty() {
        return vec![];
    }

    let (root, root_stack) = &states_stack[0];
    let new_root = root.as_ref().borrow().copy_tree();
    let mut new_states_stack = vec![(new_root.clone(), root_stack.clone())];

    let mut parent = new_root;

    for (tree, sym_stack) in states_stack.iter().skip(1) {
        // 根据旧的AST的sym，查找新的AST
        // 原理是statesstack每个元素的AST是层层相连的
        // (ith AST 是 i+1th AST的直接父母)
        let new_tree = parent
            .as_ref()
            .borrow()
            .get_elem(tree.as_ref().borrow().sym())
            .unwrap()
            .get_ast()
            .unwrap()
            .clone();

        new_states_stack.push((new_tree.clone(), sym_stack.clone()));

        parent = new_tree;
    }

    new_states_stack
}

lazy_static! {
    pub static ref PARSER: LL1Parser = LL1Parser::new(barelang_gram());
}


#[cfg(test)]
mod test {
    use crate::syntax_parser::Parser;

    #[test]
    fn test_synax() {
        use std::path::PathBuf;
        use crate::lexer::{
            SrcFileInfo
        };
        use super::PARSER;


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
