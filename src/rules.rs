
use lazy_static::lazy_static;

use crate::*;
use crate::gram::Gram;
use crate::lexer::{
    CharMatcher, RegexCharMatcher, SimpleCharMatcher,
    LexDFAMapType,
    TokenMatcherVec, RegexTokenMatcher
};

////////////////////////////////////////////////////////////////////////////////
//// Tokenizer-0 (Only Split)

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

pub fn barelang_lexdfamap() -> LexDFAMapType {
    lexdfamap! {
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
    }
}



////////////////////////////////////////////////////////////////////////////////
//// Tokenizer-1 (Recognize)

pub fn barelang_token_matcher_vec() -> TokenMatcherVec {
    token_analyzer! {
        id     => "^[[:alnum:]_]+.*$",
        intlit => r"^[+|-]?(([0-9]+)|(0x[0-9a-f]+))$",

        slash_block_comment => r"^/\*.*$",
        slash_line_comment  => r"^//.*$",
        sp => r"^[[:space:]]+$",

        paren  => r"[()\[\]{}]",
        sub    => r"-",
        add    => r"\+",
        mul    => r"\*",
        div    => r"^/$",
        semi   => r"^;$",
        dot    => r"^\.$",
        comma  => r",",
        eq     => r"=",
        klet   => r"let"
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Grammar

pub fn barelang_gram() -> Gram {
    declare_nonterminal! {
        prog,
        block,
        block_statements,
        expression,
        sum,
        product,
        addend,
        pri,
        multiplier,
        statement,
        block_statement,
        algb_pri,
        bop,
        expression_1,
        function_call,
        expression_list
    };
    declare_terminal! {
        // key

        id,
        intlit,
        paren,
        sub,
        add,
        mul,
        div,
        semi,
        dot,
        comma
    };


    use_epsilon!(ε);

    let mut barelang = grammar![barelang|
        prog:
        | block;
        | block_statements;

        block:
        | paren block_statements paren;

        block_statements:
        | block_statement block_statements;
        | ε;

        block_statement:
        | statement;

        statement:
        | semi;
        | expression;

        expression:
        | sum;
        | pri expression_1;
        | function_call;

        expression_1:
        | dot function_call expression_1;
        | bop pri expression_1;
        | ε;

        function_call:
        | id paren expression_list paren;

        expression_list:
        | expression;
        | expression comma expression_list;
        | ε;
    |];

    let sum_rule = grammar![sum|
        sum:
        | product addend;

        product:
        | algb_pri multiplier;

        algb_pri:
        | id;
        | intlit;
        | paren sum paren;

        multiplier:
        | mul algb_pri multiplier;
        | div algb_pri multiplier;
        | ε;

        addend:
        | add product addend;
        | sub product addend;
        | ε;
    |];

    barelang.extend_gram(sum_rule);

    barelang
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

        // Test Patrten OR
        // [[+|-]?[[:digit:]]+|[+|-]?0x[[:xdigit:]]+]
        matcher = Regex::new(r"abc|def").unwrap();
        assert!(matcher.is_match("abc"));
        assert!(matcher.is_match("def"));
        assert!(!matcher.is_match("dab"));
        assert!(!matcher.is_match("ade"));

        // test intlit
        matcher = Regex::new(r"^[+|-]?(([0-9]+)|(0x[0-9a-f]+))$").unwrap();
        assert!(matcher.is_match("1234"));
        assert!(matcher.is_match("-1234"));
        assert!(matcher.is_match("-0x1af4"));
        assert!(!matcher.is_match("1af4"));

        // test paren
        matcher = Regex::new(r"[()\[\]{}]").unwrap();
        assert!(matcher.is_match("["));
        assert!(matcher.is_match("("));
        assert!(matcher.is_match("]"));

        // test slash block comment
        matcher = Regex::new(r"/\*.*").unwrap();
        assert!(matcher.is_match("/*  asasasa*/"));
        assert!(!matcher.is_match("/ / aaaa"));
    }
}