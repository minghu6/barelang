
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
pub fn compile(codestr: &str, output: &str) -> Result<(), Box<dyn Error>> {
    match (*PARSER).parse(&codestr) {
        Ok(ast) => {
            println!("AST:\n{}", ast.as_ref().borrow());
            let frames = analyze_semantic(ast);
            println!("StackFrames:\n{:#?}", frames);
            codegen(frames, output)?;
            Ok(())
        },
        Err(msg) => {
            eprintln!("{}", msg);
            Err(BaCErr::new_box_err(&msg))
        }
    }
}
