
use itertools::Itertools;
use regex::Regex;
use lazy_static::lazy_static;

use std::{fmt, vec};

use crate::gram::*;
use crate::*;


////////////////////////////////////////////////////////////////////////////////
/////// Lexer

/// Token
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
make_char_matcher_rules! {
    ident       => "[[:alnum:]_]"    | r,
    ident_head  => "[[:alpha:]_]"    | r,
    delimiter   => "[,|;]"           | r,
    num         => "[[:digit:]]"     | r,
    numsign     => "[+|-]"           | r,
    op          => r#"[\+|-|\*|/|%|\^|\||&|~|!|?|:|@|>|=|<|\.]"# | r,
    any         => r#"[\d\D]"#       | r,
    ng          => r#"[^[:graph:]]"# | r,
    sp          => r#"[[:space:]]"#  | r,
    singlequote => "'"               | n,
    doublequote => "\""              | n,
    slash       => "/"               | n,
    parenthesis => r#"[\[\]{}\(\)]"# | r,
    zero        => "0"               | n,
    colon       => ":"               | n,
    bslash      => "\\"              | n,
    question    => "?"               | n,
    eq          => "="               | n,
    lt          => "<"               | n,
    asterisk    => "*"               | n,
    anybutstarslash => r#"[^[*/]]"#  | r,
    anybutbslashsq  => r#"[^['\\]]"# | r,
    anybutbslashdq  => r#"[^["\\]]"# | r,
    anybutstar  => r#"[^[*]]"#       | r,
    newline     => r#"[\n\r]"#       | r,
    anybutnewline => r#"[^[\n\r]]"#  | r,
    x           => "x"               | n,
    hex         => "[[:xdigit:]]"    | r,
    sharp       => "#"               | n
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum LexSt {
    Entry,
    Blank,
    IdentHead,
    Ident,
    SplIdent,
    SingleQuoteStr,
    SingleQuoteEnd,
    SingleQuoteStrBSlash,
    Delimiter,
    Parenthesis,
    DoubleQuoteStr,
    DoubleQuoteEnd,
    DoubleQuoteStrBSlash,
    IdentName,
    Op,
    NumZero,
    NumZeroHead,
    NumHead,
    HexNum,
    Num,
    CommentStart,
    BlkComment,
    BlkCommentEnd,
    BlkCommentEnd2,
    LnComment
}


pub type LexDFAMapType = Vec<(LexSt, Vec<(fn(&char) -> bool, (LexSt, bool))>)>;

lazy_static! {
    /// 为方便起见, matcher允许存在交集, 顺序决定优先级
    pub static ref LEX_DFA_MAP: LexDFAMapType = lexdfamap! {
        LexSt::Entry => {
            slash_m       | LexSt::CommentStart,   false
            sp_m          | LexSt::Blank,          false
            ident_head_m  | LexSt::IdentName,      false
            parenthesis_m | LexSt::Parenthesis,    false
            delimiter_m   | LexSt::Delimiter,      false
            singlequote_m | LexSt::SingleQuoteStr, false
            doublequote_m | LexSt::DoubleQuoteStr, false
            zero_m        | LexSt::NumZeroHead,    false
            num_m         | LexSt::Num,            false
            numsign_m     | LexSt::NumHead,        false
            op_m          | LexSt::Op,             false
        },
        LexSt::Blank => {
            slash_m       | LexSt::CommentStart,   true
            sp_m          | LexSt::Blank,          false
            ident_head_m  | LexSt::IdentName,      true
            parenthesis_m | LexSt::Parenthesis,    true
            delimiter_m   | LexSt::Delimiter,      true
            singlequote_m | LexSt::SingleQuoteStr, true
            doublequote_m | LexSt::DoubleQuoteStr, true
            zero_m        | LexSt::NumZeroHead,    true
            num_m         | LexSt::Num,            true
            numsign_m     | LexSt::NumHead,        true
            op_m          | LexSt::Op,             true
        },
        LexSt::IdentHead => {
            ident_head_m | LexSt::IdentName, false
        },
        LexSt::SingleQuoteStr => {
            anybutbslashsq_m | LexSt::SingleQuoteStr,       false
            bslash_m         | LexSt::SingleQuoteStrBSlash, false
            singlequote_m    | LexSt::SingleQuoteEnd,       false
        },
        LexSt::SingleQuoteEnd => {
            sp_m          | LexSt::Blank,       true
            delimiter_m   | LexSt::Delimiter,   true
            parenthesis_m | LexSt::Parenthesis, true
            colon_m       | LexSt::Delimiter,   true
        },
        LexSt::SingleQuoteStrBSlash => {
            any_m | LexSt::SingleQuoteStr, false
        },
        LexSt::Delimiter => {
            sp_m         | LexSt::Blank, true
            slash_m      | LexSt::CommentStart, true
            delimiter_m  | LexSt::Delimiter, true
            ident_head_m | LexSt::IdentName, true
        },
        LexSt::Parenthesis => {
            parenthesis_m | LexSt::Parenthesis,    true
            sp_m          | LexSt::Blank,          true
            delimiter_m   | LexSt::Delimiter,      true
            ident_head_m  | LexSt::IdentName,      true
            singlequote_m | LexSt::SingleQuoteStr, true
            doublequote_m | LexSt::DoubleQuoteStr, true

            zero_m        | LexSt::NumZeroHead,    true
            num_m         | LexSt::Num,            true
            numsign_m     | LexSt::NumHead,        true

            slash_m       | LexSt::CommentStart,   true

            op_m          | LexSt::Op,             true
        },
        LexSt::DoubleQuoteStr => {
            anybutbslashdq_m | LexSt::DoubleQuoteStr,       false
            bslash_m        | LexSt::DoubleQuoteStrBSlash, false
            doublequote_m   | LexSt::DoubleQuoteEnd,       false
        },
        LexSt::DoubleQuoteEnd => {
            sp_m          | LexSt::Blank,       true
            delimiter_m   | LexSt::Delimiter,   true
            parenthesis_m | LexSt::Parenthesis, true
        },
        LexSt::DoubleQuoteStrBSlash => {
            any_m | LexSt::DoubleQuoteStr, false
        },
        LexSt::IdentName => {
            ident_m       | LexSt::IdentName, false
            sp_m          | LexSt::Blank,     true
            parenthesis_m | LexSt::Blank,     true
            delimiter_m   | LexSt::Delimiter, true

            sharp_m       | LexSt::SplIdent,  false
            op_m          | LexSt::Op,        true
        },
        LexSt::SplIdent => {
            ident_head_m  | LexSt::IdentName, false
        },
        LexSt::Op => {
            op_m          | LexSt::Op,          false

            zero_m        | LexSt::NumZeroHead, true
            num_m         | LexSt::Num,         true
            numsign_m     | LexSt::NumHead,     true

            sp_m          | LexSt::Blank,       true
            ident_head_m  | LexSt::IdentName,   true
            delimiter_m   | LexSt::Delimiter,   true
            parenthesis_m | LexSt::Parenthesis, true
        },
        LexSt::NumHead => {
            zero_m        | LexSt::NumZeroHead, false
            num_m         | LexSt::Num,         false
            parenthesis_m | LexSt::Blank,       true
            delimiter_m   | LexSt::Delimiter,   true
            sp_m          | LexSt::Blank,       true
            op_m          | LexSt::Op,          true
        },
        LexSt::NumZeroHead => {
            x_m           | LexSt::HexNum,    false
            num_m         | LexSt::Num,       false
            parenthesis_m | LexSt::Blank,     true
            delimiter_m   | LexSt::Delimiter, true
            sp_m          | LexSt::Blank,     true
            op_m          | LexSt::Op,        true
        },
        LexSt::HexNum => {
            hex_m         | LexSt::HexNum,    false
            parenthesis_m | LexSt::Blank,     true
            delimiter_m   | LexSt::Delimiter, true
            sp_m          | LexSt::Blank,     true
            op_m          | LexSt::Op,        true
        },
        LexSt::Num => {
            num_m         | LexSt::Num,       false
            parenthesis_m | LexSt::Blank,     true
            delimiter_m   | LexSt::Delimiter, true
            sp_m          | LexSt::Blank,     true
            op_m          | LexSt::Op,        true
        },
        LexSt::CommentStart => {
            slash_m    | LexSt::LnComment,  false
            asterisk_m | LexSt::BlkComment, false
            sp_m       | LexSt::Blank,      true
            eq_m       | LexSt::Blank,      true
        },
        LexSt::BlkComment => {
            asterisk_m   | LexSt::BlkCommentEnd, false
            anybutstar_m | LexSt::BlkComment,    false
        },
        LexSt::BlkCommentEnd => {
            anybutstarslash_m | LexSt::BlkComment,     false
            asterisk_m        | LexSt::BlkCommentEnd,  false
            slash_m           | LexSt::BlkCommentEnd2, false
        },
        LexSt::BlkCommentEnd2 => {
            sp_m          | LexSt::Blank,          true
            slash_m       | LexSt::CommentStart,   true
            ident_head_m  | LexSt::IdentName,      true
            delimiter_m   | LexSt::Delimiter,      true
            parenthesis_m | LexSt::Parenthesis,    true
            singlequote_m | LexSt::SingleQuoteStr, true
            doublequote_m | LexSt::DoubleQuoteStr, true

            zero_m        | LexSt::NumZeroHead,    true
            num_m         | LexSt::Num,            true
            numsign_m     | LexSt::NumHead,        true

            op_m          | LexSt::Op,             true
        },
        LexSt::LnComment => {
            anybutnewline_m | LexSt::LnComment, false
            newline_m       | LexSt::Blank,     true
        }
    };
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

/// Token Matcher
make_token_matcher_rules! {
    ident => "[[[:alpha:]_][[:alnum:]_]*]",
    str   => ""
}

////////////////////////////////////////////////////////////////////////////////
//// Lexer

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
                println!("multiple choice: >>> {}", c);
                matched_items.iter().for_each(|(_m, (st, _f))| {
                    println!("{:?}", st);
                });
                println!("^^^");

                found_item = matched_items.first().unwrap();
                // unreachable!()
            }
        }

        // println!("{:?} => {:?}", cur_st, found_item.1.0.clone());

        // unpack found item
        cur_st = found_item.1.0.clone();
        let is_tok_end = found_item.1.1;

        if is_tok_end {
            // TODO: token matcher
            // Construct Token
            // str
            println!("{}", cache);
            cache = String::new();
        }

        cache.push(c);
    }

    // tail recycle
    println!("{}", cache);

    tokens
}






#[cfg(test)]
mod test {
    #[test]
    fn test_regex() {
        use regex::Regex;

        use super::slash_m;

        let mut matcher = Regex::new(r#"[^[ ]]"#).unwrap();

        // test ngsp
        assert!(!matcher.is_match(" "));

        matcher = Regex::new(r#"[\[\]{}\(\)]"#).unwrap();
        assert!(matcher.is_match("["));
        assert!(matcher.is_match("}"));
        assert!(!matcher.is_match("a"));

        matcher = Regex::new(r#"[^[*/]]"#).unwrap();
        assert!(matcher.is_match("a"));
        assert!(!matcher.is_match("*"));
        assert!(!matcher.is_match("/"));

        assert!(slash_m(&'/'));
        assert!(!slash_m(&'a'));

        matcher = Regex::new("[[:xdigit:]_]").unwrap();
        assert!(matcher.is_match("a"));
        assert!(!matcher.is_match("*"));
        assert!(matcher.is_match("_"));

    }

    #[test]
    fn test_lex() {
        use crate::lexer::tokenize;

        use std::fs;

        let data0 = fs::read_to_string("./examples/exp0.bare").expect("Unable to read file");

        tokenize(&data0);
    }
}