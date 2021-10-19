//! Specific grammar rules for barelang

use indexmap::{IndexMap, indexmap};
use lazy_static::lazy_static;

use crate::*;
use crate::middleware::datair::BaBOp;
use crate::frontend::gram::Gram;
use crate::frontend::lexer::{
    CharMatcher, RegexCharMatcher, SimpleCharMatcher,
    LexDFAMapType,
    TokenMatcherVec, RegexTokenMatcher
};


////////////////////////////////////////////////////////////////////////////////
//// Tokenizer-0 (Only Split)

make_char_matcher_rules! {
    ident       => "[[:alnum:]_]"    | r,
    ident_head  => "[[:alpha:]_]"    | r,
    delimiter   => "[,|;|:]"         | r,
    num         => "[[:digit:]]"     | r,
    numsign     => "[+|-]"           | r,
    op          => r#"[\+|\-|\*|/|%|\^|\||&|~|!|?|@|>|=|<|\.]"# | r,
    any         => r#"[\d\D]"#       | r,
    ng          => r#"[^[:graph:]]"# | r,
    sp          => r#"[[:space:]]"#  | r,
    singlequote => "'"               | n,
    doublequote => "\""              | n,
    slash       => "/"               | n,
    parenthesis => r#"[\[\]{}\(\)]"# | r,
    zero        => "0"               | n,
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
    LnComment,
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
            parenthesis_m | LexSt::Parenthesis,     true
            delimiter_m   | LexSt::Delimiter, true

            sharp_m       | LexSt::SplIdent,  false
            op_m          | LexSt::Op,        true
        },
        LexSt::SplIdent => {
            ident_head_m  | LexSt::IdentName,   true
            parenthesis_m | LexSt::Parenthesis, true
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
    token_recognizer! {
        f      => "^f$",
        iter   => "^iter$",
        colon  => "^:$",
        ellipsis2 => "^\\.\\.$",

        int    => "^int$",
        float  => "^float$",
        u8     => "^u8$",
        u64    => "^u64$",
        str    => "^str$",

        id     => "^[[:alpha:]_][[:alnum:]_]*$",
        splid  => "^[[:alpha:]_][[:alnum:]_]*#$",
        intlit => r"^[+|-]?(([0-9]+)|(0x[0-9a-f]+))$",

        slash_block_comment => r"^/\*.*$",
        slash_line_comment  => r"^//.*$",
        sp => r"^[[:space:]]+$",
        dqstr => r#"^"[\s\S]*"$"#,

        lparen => r"[(]",
        rparen => r"[)]",
        lbracket => r"[\[]",
        rbracket => r"[\]]",
        lbrace => r"[\{]",
        rbrace => r"[\}]",
        sub    => r"-",
        add    => r"\+",
        mul    => r"\*",
        div    => r"^/$",
        semi   => r"^;$",
        dot    => r"^\.$",
        comma  => r",",
        eq     => r"=",
        percent=> r"%"
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Syntax

/// LL(*)
#[allow(non_snake_case)]
pub fn barelang_gram() -> Gram {
    declare_nonterminal! {
        Prog,
        Block,
        BlockStmts,
        BlockStmtsSpan,
        Item,
        Stmt,
        Expr,
        ExprRem,
        Pri,
        Parameters,
        ParameterList,
        ParameterListRem,
        Parameter,
        Arguments,
        TypedId,
        TailOptionExpr,
        Declare,
        Defn,
        FunCall,
        ExprList,
        ExprListRem,
        IterCtrlScope,
        Vector,
        Range,

        BOp,
        Lit,
        Id,
        DataType,
        BlockStmt
    };
    // 终结符全部小写 (假装是没有分部的lower camel case)
    declare_terminal! {
        /* control key */
        f,
        iter,

        /* etc sign */
        ellipsis2,

        /* primitive type key */
        int,
        u64,
        u8,
        str,
        float,

        id,
        splid,
        intlit,
        dqstr,

        /* single char */
        lparen,    // (
        rparen,    // )
        lbracket,  // [
        rbracket,  // ]
        lbrace,    // {
        rbrace,    // }

        /* Infix op */
        sub,
        add,
        mul,
        div,
        semi,
        dot,
        comma,
        eq,
        percent,  // %,
        colon
    };


    use_epsilon!(ε);

    let barelang = grammar![barelang|
        Prog:
          0 -> BlockStmts;

        Block:
          0 -> lbrace BlockStmts rbrace;

        BlockStmts:
          0 -> BlockStmtsSpan TailOptionExpr;
          2 -> ε;

        BlockStmtsSpan:
          0 -> BlockStmt BlockStmtsSpan;
          1 -> ε;

        BlockStmt:
          0 -> Item;  // defn
          1 -> Stmt;  // a=2;
          2 -> Block; // {...}

        TailOptionExpr:
          0 -> Expr;
          1 -> ε;

        Stmt:
          0 -> semi;
          1 -> Expr semi;
          2 -> Declare semi;

        Declare:
          0 -> id eq Expr;
          1 -> TypedId eq Expr;

        Item:
          0 -> Defn;

        Defn:
          0 -> f colon id Parameters colon DataType Block;

        Expr:
          0 -> Pri ExprRem;
          1 -> FunCall;
          2 -> iter IterCtrlScope Block;
          3 -> Range;

        Range:
          0 -> Pri ellipsis2 Pri;
          1 -> Pri ellipsis2;
          2 -> ellipsis2 Pri;
          3 -> ellipsis2;

        IterCtrlScope:
          0 -> lparen id colon Pri rparen;

        FunCall:
          0 -> Id Arguments;

        ExprRem:
          0 -> BOp Pri ExprRem;  // 中缀表达式仅限基本操作符, 这是为了方便起见
          1 -> ε;

        Parameters:
          0 -> lparen ParameterList rparen;

        ParameterList:
          0 -> Parameter ParameterListRem;
          1 -> ε;

        ParameterListRem:
          0 -> comma Parameter ParameterListRem;
          1 -> ε;

        Parameter:
          0 -> TypedId;

        Arguments:
          0 -> lparen ExprList rparen;
          1 -> ε;

        BOp:
          0 -> add;
          1 -> sub;
          2 -> mul;
          3 -> div;
          4 -> percent;
          5 -> dot;

        Pri:
          0 -> Lit;
          1 -> Id;
          2 -> lparen Expr rparen;
          // 3 -> lbracket ExprList rbracket;
          3 -> Vector;

        Vector:
          0 -> splid lbracket ExprList rbracket;

        ExprList:
          0 -> Expr ExprListRem;
          1 -> ε;

        ExprListRem:
          0 -> comma Expr ExprListRem;
          1 -> ε;

        TypedId:
          0 -> id colon DataType;

        Id:
          0 -> id;
          1 -> splid id;

        Lit:
          0 -> intlit;
          1 -> dqstr;

        DataType:
          0 -> id;
          1 -> int;
          2 -> u64;
          3 -> u8;
          4 -> float;
          5 -> str;

    |];


    barelang
}

/// Binary Operator Precedence Map
pub fn bopprecmap() -> IndexMap<BaBOp, usize> {
    indexmap! {
        BaBOp::Add => 90,
        BaBOp::Sub => 90,

        BaBOp::Mul => 100,
        BaBOp::Div => 100,
        BaBOp::Mod => 100
    }
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

        // test double quote string
        matcher = Regex::new(r#""[\s\S]*""#).unwrap();
        assert!(matcher.is_match("\"asasas\""));
        assert!(matcher.is_match("\"你哈， 哈哈哈哈！\n\""));
        assert!(matcher.is_match("\"你哈， 哈哈哈哈！\n

这是真的好
        \""));

    }
}
