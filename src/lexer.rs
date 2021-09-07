
use itertools::Itertools;
use regex::Regex;
use lazy_static::lazy_static;

use std::fmt::Debug;
use std::path::PathBuf;
use std::{fmt, vec};
use std::fs;
use std::error::Error;

use crate::gram::*;
use crate::rules::{LexSt, barelang_lexdfamap, barelang_token_matcher_vec};


////////////////////////////////////////////////////////////////////////////////
//// Token

#[derive(Debug, Clone)]
pub struct Token {
    name: String,
    value: String,
    loc: SrcLoc
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn loc(&self) -> SrcLoc {
        self.loc.clone()
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
//// Source File Structure


/// SrcFileInfo
#[allow(dead_code)]
pub struct SrcFileInfo {
    /// Source file path
    path: PathBuf,

    /// lines[x]: number of total chars until lines x [x]
    /// inspired by `proc_macro2`: `FileInfo`
    lines: Vec<usize>,

    srcstr: String
}

impl SrcFileInfo {
    pub fn new(path: PathBuf) -> Result<Self, Box<dyn Error>> {
        let srcstr = fs::read_to_string(&path)?;

        let lines = Self::build_lines(&srcstr);

        Ok(Self {
            path,
            lines,
            srcstr
        })
    }

    fn build_lines(srcstr: &str) -> Vec<usize> {
        let mut lines = vec![0];
        let mut total = 0usize;

        for c in srcstr.chars() {
            total += 1;

            if c == '\n' {
                lines.push(total);
            }
        }

        lines
    }

    pub fn get_srcstr(&self) -> &str {
        &self.srcstr
    }

    pub fn get_path(&self) -> &PathBuf {
        &self.path
    }

    pub fn offset2srcloc(&self, offset: usize) -> SrcLoc {
        match self.lines.binary_search(&offset) {
            Ok(found) => {
                SrcLoc {
                    ln: found,
                    col: 0  // 换行处
                }
            },
            Err(idx) => {
                SrcLoc {
                    ln: idx,
                    col: offset - self.lines[idx - 1]  // 显然idx >= 0
                }
            }
        }
    }
}


#[derive(Clone)]
pub struct SrcLoc {
    pub ln: usize,
    pub col: usize
}

impl SrcLoc {
    pub fn new(loc_tuple: (usize, usize)) -> Self {
        Self {
            ln: loc_tuple.0,
            col: loc_tuple.1
        }
    }
}

impl Debug for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.ln, self.col)
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
fn gen_token(word: &str, srcloc: SrcLoc) -> Result<Token, String> {
    for (matcher, tokn) in (*TOKEN_MATCHER_VEC).iter() {
        if matcher.is_match(word) {
            return Ok(Token {
                name: tokn.clone(),
                value: word.to_owned(),
                loc: srcloc
            });
        }
    }

    Err(format!("Unknowed word type: {}", word))
}


pub fn tokenize(srcfile: &SrcFileInfo) -> Vec<Token> {
    let source = srcfile.get_srcstr();

    let mut tokens = vec![];

    if source.len() == 0 {
        return tokens;
    }

    let mut cur_st = LexSt::Entry;
    let mut cache = String::new();
    let mut total = 0usize;

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
                unreachable!(
                    format!(
                    "cur_st: {:?}, input:<{}>", cur_st, c
                    )
                )
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
            let tok = gen_token(
                &cache,
                srcfile.offset2srcloc(total + 1 - cache.chars().count())
                ).unwrap();

            // if tok.name() == "dqstr" {
            //     println!("{:#?}: len: {}, total: {}", tok, cache.len(), total);
            // }

            tokens.push(tok);

            cache.clear();
        }

        cache.push(c);

        total += 1;
    }

    // tail recycle
    // println!("{}", cache);
    let tok = gen_token(
        &cache,
        srcfile.offset2srcloc(total + 1 - cache.len())
    ).unwrap();

    // println!("{}", tok);
    tokens.push(tok);

    tokens
}






#[cfg(test)]
mod test {

    #[test]
    fn test_lex() {
        use std::path::PathBuf;
        use itertools::Itertools;
        use crate::lexer::{
            tokenize, SrcFileInfo
        };

        let srcfile
        = SrcFileInfo::new(PathBuf::from("./examples/exp0.ba")).unwrap();

        let tokens = tokenize(&srcfile);

        let trimed_tokens
        = tokens.into_iter().filter(|tok| {
            let token_name = tok.name();

            !(token_name == "sp" || token_name.ends_with("comment"))
        }).collect_vec();

        println!("{:#?}", trimed_tokens);
    }
}