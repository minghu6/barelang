
use std::error::Error;

use crate::syntax_parser::{
    PARSER, Parser
};
use crate::semantic_analyzer::{
     analyze_semantic
};
use crate::codegen::{
    codegen
};
use crate::error::{
    BaCErr
};


/// Compile source code string
pub fn compile(source: &str) -> Result<(), Box<dyn Error>> {
    match (*PARSER).parse(&source) {
        Ok(ast) => {
            println!("AST:\n{}", ast.as_ref().borrow());
            let frames = analyze_semantic(ast);
            println!("StackFrames:\n{:#?}", frames);
            codegen(frames)?;
            Ok(())
        },
        Err(msg) => {
            eprintln!("{}", msg);
            Err(BaCErr::new(&msg))
        }
    }
}
