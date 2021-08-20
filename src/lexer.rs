
use itertools::Itertools;
use regex::Regex;
use lazy_static::lazy_static;

use std::{fmt, vec};

use crate::gram::*;
use crate::rules::{LexSt, barelang_lexdfamap, barelang_token_matcher_vec};


////////////////////////////////////////////////////////////////////////////////
//// Token

#[derive(Debug, Clone)]
pub struct Token {
    name: String,
    value: String,
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn to_fst_set_sym(&self) -> FstSetSym {
        FstSetSym::Sym(self.name.clone())
    }

    /// To GramSym::Terminal
    pub fn to_gram_sym(&self) -> GramSym {
        GramSym::Terminal(self.name.clone())
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        PredSetSym::Sym(self.name.clone())
    }

    pub fn to_foll_set_sym(&self) -> FollSetSym {
        FollSetSym::Sym(self.name.clone())
    }
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.to_gram_sym(), self.value())
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Char Matcher (used for string splitter)

pub trait CharMatcher {
    fn is_match(&self, c: &char) -> bool;
}

/// Simple Char Matcher
pub struct SimpleCharMatcher {
    target: char
}

impl SimpleCharMatcher {
    pub fn new(s: &str) -> Self {
        Self {
            target: s.chars().nth(0).unwrap()
        }
    }
}

impl CharMatcher for SimpleCharMatcher {
    #[inline]
    fn is_match(&self, c: &char) -> bool {
        self.target == *c
    }
}

pub struct RegexCharMatcher {
    pat: Regex
}

impl RegexCharMatcher {
    pub fn new(patstr: &str) -> Self {
        Self {
            pat: Regex::new(patstr).unwrap()
        }
    }
}

impl CharMatcher for RegexCharMatcher {
    #[inline]
    fn is_match(&self, c: &char) -> bool {
        self.pat.is_match(&c.to_string())
    }
}


//
// Char Matcher
//
pub type LexDFAMapType = Vec<(LexSt, Vec<(fn(&char) -> bool, (LexSt, bool))>)>;

lazy_static! {
    /// 为方便起见, matcher允许存在交集, 顺序决定优先级
    pub static ref LEX_DFA_MAP: LexDFAMapType = barelang_lexdfamap();
}


////////////////////////////////////////////////////////////////////////////////
//// Token Matcher
pub trait TokenMatcher {
    fn is_match(&self, c: &str) -> bool;
}

pub struct RegexTokenMatcher {
    pat: Regex
}

impl RegexTokenMatcher {
    pub fn new(patstr: &str) -> Self {
        Self {
            pat: Regex::new(patstr).unwrap()
        }
    }
}

impl TokenMatcher for RegexTokenMatcher {
    #[inline]
    fn is_match(&self, c: &str) -> bool {
        self.pat.is_match(&c.to_string())
    }
}

pub type TokenMatcherVec = Vec<(RegexTokenMatcher, String)>;

lazy_static! {
    /// 为方便起见, matcher允许存在交集, 顺序决定优先级
    pub static ref TOKEN_MATCHER_VEC: TokenMatcherVec = barelang_token_matcher_vec();
}

////////////////////////////////////////////////////////////////////////////////
//// Lexer
fn gen_token(word: &str) -> Result<Token, String> {
    for (matcher, tokn) in (*TOKEN_MATCHER_VEC).iter() {
        if matcher.is_match(word) {
            return Ok(Token {
                name: tokn.clone(),
                value: word.to_owned()
            });
        }
    }

    Err(format!("Unknowed word type: {}", word))
}


pub fn tokenize(source: &str) -> Vec<Token> {

    let mut tokens = vec![];

    if source.len() == 0 {
        return tokens;
    }

    let mut cur_st = LexSt::Entry;
    let mut cache = String::new();

    for c in source.chars() {
        let items
        = &(*LEX_DFA_MAP).iter()
            .find(|blk| blk.0 == cur_st)
            .unwrap()
            .1;

        let matched_items
        = items.iter()
            .filter(|(matcher, _res)| matcher(&c))
            .collect_vec();

        let found_item;
        match matched_items.len() {
            0 => {
                unreachable!()
            },
            1 => {
                found_item = matched_items.first().unwrap();
            },
            _ => {
                // println!("multiple choice: >>> {}", c);
                // matched_items.iter().for_each(|(_m, (st, _f))| {
                //     println!("{:?}", st);
                // });
                // println!("^^^");

                found_item = matched_items.first().unwrap();
            }
        }

        // println!("{:?} => {:?}", cur_st, found_item.1.0.clone());

        // unpack found item
        cur_st = found_item.1.0.clone();
        let is_tok_end = found_item.1.1;

        if is_tok_end {
            // println!("{}", cache);
            let tok = gen_token(&cache).unwrap();
            // println!("{}", tok);
            tokens.push(tok);

            cache.clear();
        }

        cache.push(c);
    }

    // tail recycle
    // println!("{}", cache);
    let tok = gen_token(&cache).unwrap();
    // println!("{}", tok);
    tokens.push(tok);

    tokens
}






#[cfg(test)]
mod test {
    use itertools::Itertools;

    #[test]
    fn test_lex() {
        use crate::lexer::tokenize;
        use std::fs;

        let data0 = fs::read_to_string("./examples/exp0.ba").expect("Unable to read file");

        let tokens = tokenize(&data0);

        let trimed_tokens
        = tokens.into_iter().filter(|tok| {
            let token_name = tok.name();

            !(token_name == "sp" || token_name.ends_with("comment"))
        }).collect_vec();

        println!("{:#?}", trimed_tokens);
    }
}