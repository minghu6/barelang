use std::error::Error;

use crate::make_simple_error_rules;
use crate::datair::BaId;

////////////////////////////////////////////////////////////////////////////////
//// Define Error

make_simple_error_rules!(RuleErr);
make_simple_error_rules!(BaCErr);
make_simple_error_rules!(CLIErr);


////////////////////////////////////////////////////////////////////////////////
//// Trap Code

#[derive(Debug)]
pub enum TrapCode<'a> {
    /* BaCErr */
    UnresolvedSymbol(&'a BaId),
    ToBaiscTypeOnVoid,
    UnsupportedBopOperand,

    /* RuleErr */
    AmbigousLLRule(String),

    /* CLIErr */
    InvalidCLIArgument(String)
}

impl<'a> TrapCode<'a> {
    pub fn emit_box_err(&self) -> Box<dyn Error> {
        match &self {
            Self::UnresolvedSymbol(id) => {
                BaCErr::new_box_err(
                    format!(
                        "Unresolved Symbol: {:?}", id
                    ).as_str()
                )
            },
            Self::ToBaiscTypeOnVoid => {
                BaCErr::new_box_err(
                    format!(
                        "{:?}", self
                    ).as_str()
                )
            },
            Self::AmbigousLLRule(msg) => {
                RuleErr::new_box_err(
                    msg
                )
            },
            Self::InvalidCLIArgument(msg) => {
                CLIErr::new_box_err(msg)
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
