use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;

use crate::make_simple_error_rules;
use crate::middleware::datair::{BaDecVal, BaId, BaPriVal};
use crate::frontend::lexer::{SrcLoc, Token};


////////////////////////////////////////////////////////////////////////////////
//// Define Error

make_simple_error_rules!(RuleErr);
make_simple_error_rules!(BaCErr);
make_simple_error_rules!(CLIErr);


////////////////////////////////////////////////////////////////////////////////
//// Trap Code

#[derive(Debug)]
pub enum TrapCode<'a> {
    /* BaCErr etc. */
    ToBaiscTypeOnVoid,
    ToCGValOnVoid,
    UnsupportedDecValToPri(&'a BaDecVal),
    UnableToInferParamType(&'a BaPriVal),

    /* BaCErr Lex */
    UnsupportedCharEscape(char),

    /* BaCErr Parse */
    UnfinishedDerivation,
    UnexpectedToken(&'a Token, &'a str),
    UnsupportedBopOperand,

    /* BaCErr Semantic Analyze */
    DuplicatedDefn(&'a BaId),
    DuplicatedDefsym(&'a BaId),
    UnableToInferType(&'a BaId),
    UnresolvedSymbol(&'a BaId),
    UnresolvedFn(&'a BaId),
    PriValTryIntoIdFailed(&'a BaPriVal),

    /* BaCErr Code Generation */
    RefTypeTryIntoPureValue,

    AssignVoid,

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
            Self::DuplicatedDefn(id)
            | Self::DuplicatedDefsym(id)
            | Self::UnableToInferType(id)
            => {
                BaCErr::new_box_err(
                    format!(
                        "{:?}: {:?}", self, id
                    ).as_str()
                )
            },
            Self::PriValTryIntoIdFailed(prival) => {
                BaCErr::new_box_err(
                    format!(
                        "{:?}: {:?}", self, prival
                    ).as_str()
                )
            },
            Self::UnexpectedToken(tok, deriv_sym) => {
                BaCErr::new_box_err(
                    format!(
                        "Unexpected token {} when deriv from: {}", tok, deriv_sym
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
            Self::UnsupportedCharEscape(c) => {
                BaCErr::new_box_err(
                    format!(
                            r"Unsupported Escaped Char \{}", c
                        ).as_str()
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


////////////////////////////////////////////////////////////////////////////////
//// Error Info Collector

pub struct ErrInfoCollector {
    err_items: Rc<RefCell<Vec<(SrcLoc, Box<dyn Error>)>>>
}

impl ErrInfoCollector {
    pub fn new() -> Self {
        Self {
            err_items: Rc::new(RefCell::new(vec![]))
        }
    }

    pub fn push(&mut self, srcloc: SrcLoc, err: Box<dyn Error>) {
        self.err_items.as_ref().borrow_mut().push((srcloc, err));
    }

    pub fn collect(&mut self) -> Rc<RefCell<Vec<(SrcLoc, Box<dyn Error>)>>> {
        let err_items = self.err_items.clone();

        self.err_items = Rc::new(RefCell::new(vec![]));

        err_items
    }

    pub fn dump_errs(&mut self) {
        let errs = self.collect();

        for (srcloc, err) in errs.as_ref().borrow().iter() {
            eprintln!("{}: {}", srcloc, err);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.err_items.as_ref().borrow().is_empty()
    }
}

thread_local! {
    pub static ERR_INFO_COLL: RefCell<ErrInfoCollector> = RefCell::new(ErrInfoCollector::new())
}

pub fn push_err(srcloc: SrcLoc, err: Box<dyn Error>) {
    ERR_INFO_COLL.with(|coll| coll.borrow_mut().push(srcloc, err))
}

pub fn err_occured() -> bool {
    ERR_INFO_COLL.with(|coll| !coll.borrow().is_empty())
}

pub fn dump_errs() {
    ERR_INFO_COLL.with(|coll| coll.borrow_mut().dump_errs())
}
