use std::any::type_name;
use std::fmt::Debug;
use std::fmt::Display;
use std::error::Error;


use crate::parser::{SrcLoc, Token};



pub fn type_of<T>(_: &T) -> &'static str {
    type_name::<T>()
}



////////////////////////////////////////////////////////////////////////////////
//// TryIntoFailed

#[derive(Debug)]
pub struct TryIntoFailed {
    pub from: String,
    pub to: String,
}

impl TryIntoFailed {
    pub fn new(from: String, to: &str) -> Self {
        Self {
            from: from,
            to: to.to_owned(),
        }
    }
}


impl Display for TryIntoFailed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.from, self.to)
    }
}

impl Error for TryIntoFailed {}




////////////////////////////////////////////////////////////////////////////////
//// TryFromFailed

#[derive(Debug)]
pub struct TryFromFailed<T> {
    pub from: T,
    pub to: String
}

// impl<From, To: Debug> Display for TryFromFailed<From, To> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         if f.alternate() {
//             write!(f, "{} => {:#?}", type_of(&self.from), self.to)
//         }
//         else {
//             write!(f, "{} => {:?}", type_of(&self.from), self.to)
//         }
//     }
// }

// impl <From, To: Debug> Error for TryFromFailed<From, To> {}



////////////////////////////////////////////////////////////////////////////////
//// LispParserError

pub struct LispParserError {
    msg: String
}

impl std::fmt::Display for LispParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::fmt::Debug for LispParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::error::Error for LispParserError {}

impl LispParserError {
    pub fn new_box_err(msg: &str) -> Box<dyn std::error::Error> {
        Box::new(Self::new(msg))
    }

    pub fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string()
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//// Trap Code

#[allow(unused)]
#[derive(Debug)]
pub(crate) enum TrapCode<'a> {
    UnreconizedToken(SrcLoc),
    UnfinishedToken(SrcLoc),
    InvalidLit(SrcLoc),
    UnfinishedDerivation,
    UnsupportedEscapeChar(SrcLoc, char),
    UnexpectedToken(&'a Token, &'a str),
    TryIntoFailed(),
    UnsupportedIntLitTypeSuffix(SrcLoc, &'a str)
}

impl<'a> TrapCode<'a> {
    pub(crate) fn emit_box_err(&self) -> Box<dyn std::error::Error> {
        match &self {
            _ => {
                LispParserError::new_box_err(
                    format!(
                        "{:#?}", self
                    ).as_str()
                )
            }
        }
    }
}
