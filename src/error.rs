use std::error::Error;
use std::fmt;

use crate::datair::BaId;

#[derive(Debug)]
pub struct BaCErr {
    msg: String
}

impl fmt::Display for BaCErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for BaCErr {}


impl BaCErr {
    pub fn new_box_err(msg: &str) -> Box<dyn Error> {
        Box::new(Self::new(msg))
    }

    pub fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string()
        }
    }
}


#[derive(Debug)]
pub enum TrapCode<'a> {
    UnresolvedSymbol(&'a BaId),
    ToBaiscTypeOnVoid,
    UnsupportedBopOperand
}

impl<'a> TrapCode<'a> {
    pub fn emit_box_err(&self) -> Box<dyn Error> {
        match self {
            &Self::UnresolvedSymbol(id) => {
                BaCErr::new_box_err(
                    format!(
                        "Unresolved Symbol: {:?}", id
                    ).as_str()
                )
            },
            &Self::ToBaiscTypeOnVoid => {
                BaCErr::new_box_err(
                    format!(
                        "{:?}", self
                    ).as_str()
                )
            },
            _ => {
                BaCErr::new_box_err(
                    format!(
                        "{:#?}", self
                    ).as_str()
                )
            }
        }
    }
}
