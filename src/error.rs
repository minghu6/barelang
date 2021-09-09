use std::error::Error;

use crate::make_simple_error_rules;
use crate::datair::BaId;

////////////////////////////////////////////////////////////////////////////////
//// Rule Engine Error

make_simple_error_rules!(RuleErr);


////////////////////////////////////////////////////////////////////////////////
//// Bare Lang Compiler Error

make_simple_error_rules!(BaCErr);


#[derive(Debug)]
pub enum TrapCode<'a> {
    /* BaCErr */
    UnresolvedSymbol(&'a BaId),
    ToBaiscTypeOnVoid,
    UnsupportedBopOperand,

    /* RuleErr */
    AmbigousLLRule(String)
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
            }
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
