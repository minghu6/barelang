
use crate::synax_parser::{
    PARSER, Parser
};


pub fn compile(source: &str) {
    match (*PARSER).parse(&source) {
        Ok(res) => {
            println!("{}", res.as_ref().borrow())
        },
        Err(msg) => {
            eprintln!("{}", msg);
        }
    }
}
