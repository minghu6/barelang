use std::fs;
use std::error::Error;
use std::env;
use std::path::{Path, PathBuf};
use std::process::{
    Command
};
use std::time::{
    SystemTime, UNIX_EPOCH
};

use bac::ml_simplifier::MLSimplifier;
// use clap::{
//     App, Arg, SubCommand
// };
use clap::{
    clap_app
};

use bac::lexer::{
    SrcFileInfo
};
use bac::syntax_parser::{
    PARSER, Parser
};
use bac::semantic_analyzer::{
     analyze_semantic
};
use bac::codegen::{codegen};
use bac::error::{
    BaCErr
};
use bac::*;

fn unique_suffix() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
        .to_string()
}


fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap_app!(bac =>
        (version: "0.1.0")
        (author: "minghu6")
        (about: "Bare Language Compiler")
        (@arg FILE: +required "input source file (exp *.ba)")
        (@arg target_type: -t --target_type +takes_value "[bin|dylib|reloc]")
        (@arg O2: -O "Optimized code")
        (@arg output: -o --output +takes_value "object file output path")
    ).get_matches();

    /* Build File && Output */
    let source_file_path = matches.value_of("FILE").unwrap();
    let mut tmp_out_fn = String::from("__barelang_output");
    tmp_out_fn.push_str(&unique_suffix());

    let input = PathBuf::from(source_file_path);
    let file = SrcFileInfo::new(input)?;
    let objpath = PathBuf::from(tmp_out_fn.clone());

    /* Build Target Type */
    let target_type
    = if let Some(target_type) = matches.value_of("target_type") {
        match target_type {
            "bin" => {
                TargetType::Bin
            },
            _ => unimplemented!()
        }
    }
    else {
        TargetType::Bin
    };

    /* Build OptLv */
    let optlv
    = if let Some(_) = matches.value_of("O2") {
        OptLv::Opt(2)
    }
    else {
        OptLv::Debug
    };



    let compiler_config = CompilerConfig {
        optlv,
        target_type,
        file,
        objpath
    };

    compile(&compiler_config)?;

    let bare_home_str = env::var("BARE_HOME").unwrap_or(".".to_string());
    let bare_home = fs::canonicalize(Path::new(
        &bare_home_str
    ))?;

    let lib_path = bare_home.join("libbare.so");

    let output = match matches.value_of("output") {
        Some(output) => output,
        None => "a.out"
    };

    //gcc output.o libbare.so -Xlinker -rpath ./ -o main
    let gcc_link_st = Command::new("gcc")
    .arg(tmp_out_fn.as_str())
    .arg(lib_path.to_str().unwrap())
    .arg("-Xlinker")
    .arg("-rpath")
    .arg(bare_home_str)
    .arg("-o")
    .arg(output)
    .status()?;

    assert!(gcc_link_st.success(), "gcc link {}", gcc_link_st);

    fs::remove_file(tmp_out_fn)?;

    Ok(())
}


/// Compile source code string
pub fn compile(config: &CompilerConfig) -> Result<(), Box<dyn Error>> {
    match (*PARSER).parse(&config.file) {
        Ok(ast) => {
            println!("AST:\n{}", ast.as_ref().borrow());
            let ml = analyze_semantic(ast);
            println!("ML:\n{:#?}", ml);
            let mut mlslf = MLSimplifier::new();
            let bin = mlslf.simplify_ml(ml);
            println!("BaBin:\n{:#?}", bin);
            codegen(&config, &bin)?;
            Ok(())
        },
        Err(msg) => {
            eprintln!("{}", msg);
            Err(BaCErr::new_box_err(&msg))
        }
    }
}
