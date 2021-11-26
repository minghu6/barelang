use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::fs;
use std::path::Path;

use crate::data::{
    AggData, AnyData, BraceMapData, BracketTupleData, FloatData, IntData,
    KeyData, LispModule, ListData, LitData, ModuleMeta, NilData,
    NonNilListData, PriData, StrData, SymData,
};
use crate::error::TrapCode;

use bacommon::config::PrintTy;

#[derive(Default)]
pub struct LispParserConfig {
    pub tokens_output: Option<PrintTy>
}



pub struct LispParser {
    cursor: usize,
    tokens: Vec<Token>,
    meta: ModuleMeta,
}

impl LispParser {
    pub fn new(path: &Path, config: LispParserConfig) -> Result<Self, Box<dyn Error>> {
        let text = fs::read_to_string(&path)?;
        let mut lexer = LispLexer::new(text);
        let tokens = LispLexer::trim_comments(lexer.tokenize()?);

        match config.tokens_output {
            Some(PrintTy::StdErr) => {
                eprintln!("{:#?}", tokens)
            },
            Some(PrintTy::File(_path)) => todo!(),
            None => ()
        }

        let name = path.file_stem().unwrap().to_str().unwrap().to_owned();
        let path = Box::from(path);
        let meta = ModuleMeta { path, name };

        let cursor = 0;

        Ok(Self {
            cursor,
            tokens,
            meta,
        })
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Basic Parse Utils

    fn peek1(&self) -> Option<&Token> {
        if self.cursor == self.tokens.len() {
            None
        } else {
            Some(&self.tokens[self.cursor])
        }
    }

    fn peek1_t(&self, name: &str) -> bool {
        if let Some(tok) = self.peek1() {
            if tok.name() == name {
                return true;
            }
        }

        false
    }

    fn try_peek1(&self) -> Result<&Token, Box<dyn Error>> {
        if let Some(tok) = self.peek1() {
            Ok(tok)
        } else {
            Err(TrapCode::UnfinishedDerivation.emit_box_err())
        }
    }

    fn consume1(&mut self) -> Result<&Token, Box<dyn Error>> {
        if self.cursor == self.tokens.len() {
            Err(TrapCode::UnfinishedDerivation.emit_box_err())
        } else {
            let old_cursor = self.cursor;
            self.cursor += 1;
            Ok(&self.tokens[old_cursor])
        }
    }

    fn consume_t(&mut self, name: &str) -> Result<&Token, Box<dyn Error>> {
        let tok = self.consume1()?;

        if tok.name() == name {
            Ok(tok)
        } else {
            Err(TrapCode::UnexpectedToken(tok, name).emit_box_err())
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Parse

    pub fn parse(&mut self) -> Result<LispModule, Box<dyn Error>> {
        let lists = self.parse_lists()?;

        Ok(LispModule {
            meta: self.meta.clone(),
            lists,
        })
    }

    fn parse_lists(&mut self) -> Result<Vec<ListData>, Box<dyn Error>> {
        let mut lists = vec![];

        while self.peek1_t("<lparen>") {
            lists.push(self.parse_list()?);
        }

        Ok(lists)
    }

    fn parse_list(&mut self) -> Result<ListData, Box<dyn Error>> {
        let tok1st = self.consume_t("<lparen>")?;
        let loc = tok1st.loc();

        let res = self.parse_list_0(loc);
        self.consume_t("<rparen>")?;

        res
    }

    fn parse_qlist(&mut self) -> Result<ListData, Box<dyn Error>> {
        let tok1st = self.consume_t("<qlparen>")?;
        let loc = tok1st.loc();

        let res = self.parse_list_0(loc);
        self.consume_t("<rparen>")?;

        res
    }

    fn parse_list_0(
        &mut self,
        loc: SrcLoc,
    ) -> Result<ListData, Box<dyn Error>> {
        if self.peek1_t("<rparen>") {
            return Ok(ListData::Nil(NilData { loc }));
        }

        let head = box self.parse_anydata()?;

        let tail = box if self.peek1_t("<rparen>") {
            ListData::Nil(NilData { loc: loc.clone() })
        } else {
            self.parse_list_0(self.try_peek1()?.loc())?
        };

        Ok(ListData::NonNil(NonNilListData { loc, head, tail }))
    }

    fn parse_bracket_tuple(
        &mut self,
    ) -> Result<BracketTupleData, Box<dyn Error>> {
        let tok1st = self.consume_t("<lbracket>")?;
        let loc = tok1st.loc();

        let res = self.parse_bracket_tuple_0(loc);
        self.consume_t("<rbracket>")?;

        res
    }

    fn parse_bracket_tuple_0(
        &mut self,
        loc: SrcLoc,
    ) -> Result<BracketTupleData, Box<dyn Error>> {
        let mut items = vec![];

        while !self.peek1_t("<rbracket>") {
            items.push(self.parse_anydata()?);
        }

        Ok(BracketTupleData { loc, items })
    }

    fn parse_brace_map(&mut self) -> Result<BraceMapData, Box<dyn Error>> {
        let tok1st = self.consume_t("<lbrace>")?;
        let loc = tok1st.loc();

        let res = self.parse_brace_map_0(loc);
        self.consume_t("<rbrace>")?;

        res
    }

    fn parse_cfxbrace_map(&mut self) -> Result<BraceMapData, Box<dyn Error>> {
        let tok1st = self.consume_t("<cfxlbrace>")?;
        let loc = tok1st.loc();

        let res = self.parse_brace_map_0(loc);
        self.consume_t("<rbrace>")?;

        res
    }

    fn parse_brace_map_0(
        &mut self,
        loc: SrcLoc,
    ) -> Result<BraceMapData, Box<dyn Error>> {
        let mut entries = vec![];

        while !self.peek1_t("<rbrace>") {
            entries.push((self.parse_anydata()?, self.parse_anydata()?));
        }

        Ok(BraceMapData { loc, entries })
    }

    fn parse_lit(&mut self) -> Result<LitData, Box<dyn Error>> {
        let tok1st = self.consume1()?;
        let loc = tok1st.loc();

        Ok(match tok1st.name().as_str() {
            "<strlit>" => match unescape_str(tok1st.value()) {
                Ok(val) => LitData::Str(StrData { val, loc }),
                Err(err_char) => {
                    return Err(TrapCode::UnsupportedEscapeChar(
                        loc, err_char,
                    )
                    .emit_box_err());
                }
            },
            "<hexlit>" => {
                let val = parse_hexlit(tok1st.value())?;
                LitData::Int(IntData {
                    val: val as u128,
                    len: 8,
                    signed: false,
                    loc,
                })
            }
            "<intlit>" => LitData::Int(Self::parse_tok_intlit(&tok1st)?),
            "<floatlit>" => {
                let val = parse_floatlit(tok1st.value())?;
                LitData::Float(FloatData { val, len: 8, loc })
            }
            _ => unreachable!(),
        })
    }

    fn parse_tok_intlit(tok: &Token) -> Result<IntData, Box<dyn Error>> {
        let val = tok.value();

        let (signed, len, trimed_val) = if val.ends_with("u8") {
            (false, 1, val.trim_end_matches("u8"))
        } else if val.ends_with("i8") {
            (true, 1, val.trim_end_matches("i8"))
        } else if val.ends_with("i16") {
            (true, 2, val.trim_end_matches("i16"))
        } else if val.ends_with("u16") {
            (false, 2, val.trim_end_matches("u16"))
        } else if val.ends_with("i32") {
            (true, 4, val.trim_end_matches("i32"))
        } else if val.ends_with("u32") {
            (false, 4, val.trim_end_matches("u32"))
        } else if val.ends_with("i64") {
            (true, 8, val.trim_end_matches("i64"))
        } else if val.ends_with("u64") {
            (false, 8, val.trim_end_matches("u64"))
        } else if val.ends_with("i128") {
            (true, 16, val.trim_end_matches("i128"))
        } else if val.ends_with("u128") {
            (false, 16, val.trim_end_matches("u128"))
        } else {
            (true, 8, val)
        };

        let is_neg = if trimed_val.starts_with("-") {
            true
        } else {
            false
        };

        let trimed_val =
            trimed_val.trim_start_matches("+").trim_start_matches("-");

        let u128val = trimed_val.parse::<u128>()?;

        let u128val = if is_neg {
            (0i128 - u128val as i128) as u128
        } else {
            u128val
        };

        Ok(IntData {
            len,
            signed,
            val: u128val,
            loc: tok.loc(),
        })
    }

    fn parse_anydata(&mut self) -> Result<AnyData, Box<dyn Error>> {
        let tok1st = self.try_peek1()?;

        let val = tok1st.value().to_owned();
        let loc = tok1st.loc();

        Ok(match tok1st.name().as_str() {
            "<intlit>" | "<hexlit>" | "<floatlit>" | "<dqstr>" => {
                AnyData::Pri(PriData::Lit(self.parse_lit()?))
            }
            "<key>" => {
                self.consume1()?;

                AnyData::Pri(PriData::Key(KeyData { val, loc }))
            }
            "<sym>" => {
                self.consume1()?;

                AnyData::Pri(PriData::Sym(SymData { val, loc }))
            }
            "<qsym>" => {
                let quote = AnyData::Pri(PriData::Sym(SymData {
                    val: "quote".to_owned(),
                    loc: loc.clone(),
                }));

                let any_id = AnyData::Pri(PriData::Sym(SymData {
                    val: self.consume1()?.value().to_owned(),
                    loc: loc.clone(),
                }));

                AnyData::nonnil_trivial_list(quote, any_id, loc)
            }
            "<qlparen>" => {
                let quote = AnyData::Pri(PriData::Sym(SymData {
                    val: "quote".to_owned(),
                    loc: loc.clone(),
                }));

                let any_list =
                    AnyData::Agg(AggData::List(self.parse_qlist()?));

                AnyData::nonnil_trivial_list(quote, any_list, loc)
            }
            "<lparen>" => AnyData::Agg(AggData::List(self.parse_list()?)),
            "<lbrace>" => {
                AnyData::Agg(AggData::BraceMap(self.parse_brace_map()?))
            }
            "<lbracket>" => AnyData::Agg(AggData::BracketTuple(
                self.parse_bracket_tuple()?,
            )),
            "<cfxlbrace>" => {
                let meta = AnyData::Pri(PriData::Sym(SymData {
                    val: "meta".to_owned(),
                    loc: loc.clone(),
                }));
                let any_map = AnyData::Agg(AggData::BraceMap(
                    self.parse_cfxbrace_map()?,
                ));

                AnyData::nonnil_trivial_list(meta, any_map, loc)
            }
            "<cfxsym>" => {
                let meta = AnyData::Pri(PriData::Sym(SymData {
                    val: "meta".to_owned(),
                    loc: loc.clone(),
                }));

                let tag_key = AnyData::Pri(PriData::Key(KeyData {
                    val: ":tag".to_owned(),
                    loc: loc.clone(),
                }));

                let val = AnyData::Pri(PriData::Sym(SymData {
                    val: self.consume_t("<cfxsym>")?.value().to_owned(),
                    loc: loc.clone(),
                }));

                let spec_map = AnyData::Agg(AggData::BraceMap(BraceMapData {
                    entries: vec![(tag_key, val)],
                    loc: loc.clone(),
                }));

                AnyData::nonnil_trivial_list(meta, spec_map, loc)
            }
            "<shsym>" => {
                todo!()
            }
            "<shlparen>" => {
                todo!()
            }
            "<shlbrace>" => {
                todo!()
            }
            _ => unreachable!("{:#?}", tok1st),
        })
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Parse Utils

/// ```none
/// 0x1234
/// ```
fn parse_hexlit(litstr: &str) -> Result<u64, Box<dyn Error>> {
    let is_neg = if litstr.starts_with("-") { true } else { false };

    let purelitstr = litstr
        .trim_start_matches("-")
        .trim_start_matches("+")
        .trim_start_matches("0x");

    match u64::from_str_radix(purelitstr, 16) {
        Ok(res) => Ok(if is_neg { unimplemented!() } else { res }),
        Err(err) => Err(Box::new(err)),
    }
}

/// ```no_run
/// assert_eq!(parse_floatlit("0.123").unwrap(), 0.123);
/// assert_eq!(parse_floatlit("-0.123").unwrap(), -0.123);
/// assert_eq!(parse_floatlit("12.3").unwrap(), 12.3);
/// assert_eq!(parse_floatlit("4.0").unwrap(), 4.0);
/// ```
fn parse_floatlit(litstr: &str) -> Result<f64, Box<dyn Error>> {
    let is_neg = if litstr.starts_with("-") { true } else { false };

    let purelitstr = litstr.trim_start_matches("-").trim_start_matches("+");

    match purelitstr.parse::<f64>() {
        Ok(res) => Ok(if is_neg { -res } else { res }),
        Err(err) => Err(Box::new(err)),
    }
}

/// ```none
/// string unescape:
///   | \n -> 10  0x0a
///   | \r -> 13  0x0d
///   | \t -> 9   0x09
///   | \" -> 34  0x22
///   | \\ -> 92  0x5c
///   | \' -> 39  0x27
/// ```
fn unescape_str(escaped_str: &str) -> Result<String, char> {
    enum State {
        Normal,
        EscapeReady,
    }

    enum Strategy {
        PushPop,
        JustPush,
        UnEscape,
    }

    let mut st = State::Normal;
    let mut output = String::new();
    let mut strategy;
    // phantom cache

    for u in escaped_str.chars() {
        (st, strategy) = match (st, u) {
            (State::Normal, '\\') => (State::EscapeReady, Strategy::JustPush),
            (State::Normal, _) => (State::Normal, Strategy::PushPop),
            (State::EscapeReady, 'n' | 'r' | 't' | '"' | '\\' | '\'') => {
                (State::Normal, Strategy::UnEscape)
            }
            (State::EscapeReady, _) => return Err(u),
        };

        match strategy {
            Strategy::PushPop => {
                output.push(u);
            }
            Strategy::JustPush => {}
            Strategy::UnEscape => {
                output.push(match u {
                    'n' => '\u{000a}',
                    'r' => '\u{000d}',
                    't' => '\u{0009}',
                    '"' => '\u{0022}',
                    '\\' => '\u{005c}',
                    '\'' => '\u{0027}',
                    _ => unreachable!(),
                });
            }
        };
    }

    Ok(output)
}

////////////////////////////////////////////////////////////////////////////////
//// Token

#[derive(Debug, Clone)]
pub(crate) struct Token {
    name: String,
    value: String,
    loc: SrcLoc,
}

impl Token {
    pub(crate) fn new(name: String, value: String, loc: SrcLoc) -> Self {
        Self { name, value, loc }
    }

    pub(crate) fn name(&self) -> String {
        format!("<{}>", &self.name)
    }

    pub(crate) fn value(&self) -> &str {
        &self.value
    }

    pub(crate) fn loc(&self) -> SrcLoc {
        self.loc.clone()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Token Name Constant

// pub const TOK_ID: &'static str = "id";

////////////////////////////////////////////////////////////////////////////////
//// SrcLoc

#[derive(Clone, PartialEq, Eq, PartialOrd, Default)]
pub struct SrcLoc {
    pub ln: usize,
    pub col: usize,
}

impl SrcLoc {
    pub(crate) fn new(loc_tuple: (usize, usize)) -> Self {
        Self {
            ln: loc_tuple.0,
            col: loc_tuple.1,
        }
    }
}

impl Debug for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.ln, self.col)
    }
}

impl Ord for SrcLoc {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.ln == other.ln {
            self.col.cmp(&other.col)
        } else {
            self.ln.cmp(&other.ln)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Lexer

const SPEC_CHARS_SET: &'static str = "-*_+>=<!\\";

pub(crate) struct LispLexer {
    chars: Vec<char>,
    cursor: usize,
    pos: SrcLoc,
}

impl LispLexer {
    pub(crate) fn new(text: String) -> Self {
        let chars = text.chars().collect();
        let cursor = 0;

        Self {
            chars,
            cursor,
            pos: SrcLoc::new((1, 1)),
        }
    }

    pub(crate) fn trim_comments(income: Vec<Token>) -> Vec<Token> {
        income
            .into_iter()
            .filter(|tok| tok.name() != "<lncomment>")
            .collect()
    }

    fn peek1(&self) -> Option<char> {
        if self.cursor == self.chars.len() {
            None
        } else {
            Some(self.chars[self.cursor].clone())
        }
    }

    fn try_peek1(&self) -> Result<char, Box<dyn Error>> {
        if self.cursor == self.chars.len() {
            Err(TrapCode::UnfinishedToken(self.pos.clone()).emit_box_err())
        } else {
            Ok(self.chars[self.cursor].clone())
        }
    }

    fn consume1(&mut self) -> Result<char, Box<dyn Error>> {
        let optc = self.peek1();

        if let Some(c) = optc {
            self.cursor += 1;

            if c == '\n' {
                self.pos.ln += 1;
                self.pos.col = 1;
            } else if c == '\r' {
                self.pos.col = 1;
            } else {
                self.pos.col += 1;
            }

            Ok(optc.unwrap())
        } else {
            Err(TrapCode::UnfinishedToken(self.pos.clone()).emit_box_err())
        }
    }

    pub(crate) fn tokenize(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens = vec![];

        while let Some(c) = self.peek1() {
            let loc = self.pos.clone();

            let token = match c {
                '(' => Token::new(
                    "lparen".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                '[' => Token::new(
                    "lbracket".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                '{' => Token::new(
                    "lbrace".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                ')' => Token::new(
                    "rparen".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                ']' => Token::new(
                    "rbracket".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                '}' => Token::new(
                    "rbrace".to_owned(),
                    self.consume1()?.to_string(),
                    loc,
                ),
                ';' => self.tokenize_lncomment()?,
                ':' => self.tokenize_key()?,
                '\'' => self.tokenize_quote_etc()?,
                '#' => self.tokenize_sharp_etc()?,
                '^' => self.tokenize_circumflex_etc()?,
                '"' => self.tokenize_dqstr()?,
                _ => {
                    if c.is_alphabetic()
                        || SPEC_CHARS_SET.contains(&c.to_string())
                    {
                        self.tokenize_sym()?
                    } else if c.is_ascii_hexdigit() || c == '.' {
                        self.tokenize_num()?
                    } else if c.is_whitespace() {
                        self.consume1()?;
                        continue;
                    } else {
                        // dbg!(c);

                        return Err(TrapCode::UnreconizedToken(
                            self.pos.clone(),
                        )
                        .emit_box_err());
                    }
                }
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    /// `intlit | hexlit | floatlit`
    fn tokenize_num(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();

        enum NumLitTy {
            HexLit,
            IntLit,
            FloatLit,
        }

        impl Display for NumLitTy {
            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
            ) -> std::fmt::Result {
                match &self {
                    Self::HexLit => write!(f, "hexlit"),
                    Self::IntLit => write!(f, "intlit"),
                    Self::FloatLit => write!(f, "floatlit"),
                }
            }
        }

        let ty;
        let mut val = String::new();

        let c0 = self.consume1()?;

        if c0 == '-' {
            val.push(c0);
        }

        let c1 = self.consume1()?;

        if c1 == '0' {
            val.push(c1);
            let c2_opt = self.peek1();

            if let Some(c2) = c2_opt {
                val.push(self.consume1()?);

                if c2 == 'x' {
                    // tokenize hex
                    let c3 = self.consume1()?;

                    if c3.is_ascii_hexdigit() {
                        val.push(c3);
                    } else {
                        return Err(TrapCode::UnfinishedToken(
                            self.pos.clone(),
                        )
                        .emit_box_err());
                    }

                    let mut has_dot = false;
                    loop {
                        if let Some(c_nxt) = self.peek1() {
                            if c_nxt.is_ascii_digit()
                                || !has_dot && c_nxt == '.'
                            {
                                if c_nxt == '.' {
                                    has_dot = true;
                                }

                                val.push(self.consume1()?);
                                continue;
                            }
                        }

                        break;
                    }

                    ty = NumLitTy::HexLit;
                } else if c2 == '.' {
                    // tokenzie float
                    loop {
                        if let Some(c_nxt) = self.peek1() {
                            if c_nxt.is_digit(10) {
                                val.push(self.consume1()?);
                                continue;
                            }
                        }

                        break;
                    }

                    ty = NumLitTy::FloatLit;
                } else {
                    // dbg!(c0);

                    return Err(TrapCode::UnreconizedToken(self.pos.clone())
                        .emit_box_err());
                }
            } else {
                ty = NumLitTy::IntLit;
            }
        } else {
            let mut has_dot = false;
            loop {
                if let Some(c_nxt) = self.peek1() {
                    if c_nxt.is_digit(10) || !has_dot && c_nxt == '.' {
                        if c_nxt == '.' {
                            has_dot = true;
                        }

                        val.push(self.consume1()?);
                        continue;
                    }
                }

                break;
            }

            // eat type annotation suffix: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | u128 | i128
            if let Some(c_nxt) = self.peek1() {
                if c_nxt == 'u' || c_nxt == 'i' {
                    val.push(self.consume1()?);

                    if let Some(c_nxt) = self.peek1() {
                        if c_nxt == '1' {
                            val.push(self.consume1()?);

                            if let Some(c_nxt) = self.peek1() {
                                if c_nxt == '6' {
                                    val.push(self.consume1()?);
                                } else if c_nxt == '2' {
                                    val.push(self.consume1()?);
                                    val.push(self.consume1()?);
                                } else {
                                    val.push(self.consume1()?);

                                    return Err(
                                        TrapCode::UnsupportedIntLitTypeSuffix(
                                            self.pos.clone(),
                                            val.as_str(),
                                        )
                                        .emit_box_err(),
                                    );
                                }
                            }
                            val.push(self.consume1()?);
                            val.push(self.consume1()?);
                        } else if c_nxt == '8' {
                            val.push(self.consume1()?);
                        } else if c_nxt == '3' || c_nxt == '6' {
                            val.push(self.consume1()?);
                            val.push(self.consume1()?);
                        } else {
                            return Err(TrapCode::UnsupportedIntLitTypeSuffix(
                                pos.clone(),
                                val.as_str(),
                            )
                            .emit_box_err());
                        }
                    }
                }
            }

            ty = if has_dot {
                NumLitTy::FloatLit
            } else {
                NumLitTy::IntLit
            };
        }

        Ok(Token::new(ty.to_string(), val, pos))
    }

    /// `sym`
    fn tokenize_sym(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let mut val = String::new();

        let c0 = self.consume1()?;

        if c0.is_alphabetic() || SPEC_CHARS_SET.contains(&c0.to_string()) {
            //
        } else {
            return Err(
                TrapCode::UnreconizedToken(self.pos.clone()).emit_box_err()
            );
        }

        val.push(c0);

        loop {
            if let Some(c_nxt) = self.peek1() {
                if c_nxt.is_alphanumeric()
                    || SPEC_CHARS_SET.contains(&c_nxt.to_string())
                {
                    val.push(self.consume1()?);
                    continue;
                }
            }

            break;
        }

        Ok(Token::new("sym".to_owned(), val, pos))
    }

    /// `key`
    fn tokenize_key(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let mut val = String::new();

        loop {
            if let Some(c_nxt) = self.peek1() {
                if !c_nxt.is_whitespace() {
                    val.push(self.consume1()?);
                    continue;
                }
            }

            break;
        }

        Ok(Token::new("key".to_string(), val, pos))
    }

    /// `dqstrlit`
    fn tokenize_dqstr(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let mut val = String::new();

        self.consume1()?;

        let mut st = 0;
        loop {
            if let Some(c_nxt) = self.peek1() {
                if st == 0 && c_nxt == '\\' {
                    st = 1;
                } else if st == 1 {
                    st = 0;
                    val.push(match c_nxt {
                        'n' => '\u{000a}',
                        'r' => '\u{000d}',
                        't' => '\u{0009}',
                        '"' => '\u{0022}',
                        '\\' => '\u{005c}',
                        '\'' => '\u{0027}',
                        _ => {
                            return Err(TrapCode::InvalidLit(self.pos.clone())
                                .emit_box_err())
                        }
                    });
                } else if st == 0 && c_nxt == '"' {
                    break;
                }

                val.push(self.consume1()?);
                continue;
            }

            break;
        }

        self.consume1()?;

        Ok(Token::new("dqstrlit".to_string(), val, pos))
    }

    /// `lncomment`
    fn tokenize_lncomment(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let mut val = String::new();

        loop {
            if let Some(c_nxt) = self.peek1() {
                if c_nxt == ';' {
                    self.consume1()?;
                    continue;
                }
            }

            break;
        }

        loop {
            if let Some(c_nxt) = self.peek1() {
                if c_nxt != '\n' {
                    val.push(self.consume1()?);
                    continue;
                }
            }

            break;
        }

        Ok(Token::new("lncomment".to_string(), val, pos))
    }

    /// `qsym | qlparen`
    fn tokenize_quote_etc(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let val;
        let ty;

        self.consume1()?;
        let c1 = self.try_peek1()?;
        if c1 == '(' {
            val = self.consume1()?.to_string();
            ty = "qlparen"
        } else {
            let tok_sym = self.tokenize_sym()?;
            val = tok_sym.value().to_string();
            ty = "qsym"
        }

        Ok(Token::new(ty.to_string(), val, pos))
    }

    /// `shsym | shlparen | shlbrace`
    fn tokenize_sharp_etc(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let val;
        let ty;

        self.consume1()?;

        let c1 = self.consume1()?;

        if c1 == '(' {
            val = self.consume1()?.to_string();
            ty = "shlparen";
        } else if c1 == '{' {
            val = self.consume1()?.to_string();
            ty = "shlbrace";
        } else {
            let tok_sym = self.tokenize_sym()?;
            val = tok_sym.value().to_string();
            ty = "shsym"
        }

        Ok(Token::new(ty.to_string(), val, pos))
    }

    /// `cfxid | cfxlbrace`
    fn tokenize_circumflex_etc(&mut self) -> Result<Token, Box<dyn Error>> {
        let pos = self.pos.clone();
        let val;
        let ty;

        self.consume1()?;

        let c1 = self.consume1()?;

        if c1 == '{' {
            val = self.consume1()?.to_string();
            ty = "cfxlbrace";
        } else {
            let tok_sym = self.tokenize_sym()?;
            val = tok_sym.value().to_string();
            ty = "cfxsym"
        }

        Ok(Token::new(ty.to_string(), val, pos))
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::error::Error;
    use std::fs;
    use std::path::Path;

    use crate::{
        parser::{parse_hexlit, LispParser, LispParserConfig},
        pretty_printer::Dump,
    };

    use super::LispLexer;

    #[test]
    fn test_lex() -> Result<(), Box<dyn Error>> {
        let path = "./examples/arr.core.lisp";
        let text = fs::read_to_string(&path)?;

        let mut lexer = LispLexer::new(text);
        let tokens = lexer.tokenize()?;
        println!("{:#?}", tokens);

        Ok(())
    }

    #[test]
    fn test_parser() -> Result<(), Box<dyn Error>> {
        let path = "./examples/arr.core.lisp";

        let mut parser = LispParser::new(
            Path::new(path),
            LispParserConfig::default()
        )?;
        let lispmodule = parser.parse()?;

        // println!("{:#?}", lispmodule);
        println!("{}", (box lispmodule as Box<dyn Dump>));

        Ok(())
    }

    #[test]
    fn test_parse_utils() -> Result<(), Box<dyn Error>> {
        assert_eq!(parse_hexlit("0x123").unwrap(), 0x123);

        Ok(())
    }
}
