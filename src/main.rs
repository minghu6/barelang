use std::fs;
use std::error::Error;

// use clap::{
//     App, Arg, SubCommand
// };
use clap::{
    clap_app
};

use bac::compiler::{
    compile
};

fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap_app!(_bac =>
        (version: "0.1.0")
        (author: "minghu6")
        (about: "Inner Bare Language Compiler")
        (@arg FILE: +required "input source file (exp *.ba)")
    ).get_matches();

    let source_file_path = matches.value_of("FILE").unwrap();
    let codestr
    = fs::read_to_string(source_file_path)
        .expect(&format!("Unable to read file {}", source_file_path));


    compile(&codestr)?;

    Ok(())
}
