use std::fs;
use std::error::Error;
use std::path::{ PathBuf};
use std::time::{
    SystemTime, UNIX_EPOCH
};

use bac::middleware::ml_simplifier::MLSimplifier;
use bac::utils::PrintTy;
// use clap::{
//     App, Arg, SubCommand
// };
use clap::{
    clap_app
};

use bac::frontend::lexer::{SrcFileInfo, tokenize};
use bac::frontend::manual_parser::{
    Parser
};
use bac::backend::codegen::{
    codegen
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
        (@arg target_type: -t --target +takes_value "[bin|dylib|reloc] default: bin")
        (@arg emit_type: -e --emit +takes_value "[llvm-ir|asm|obj] default: obj")
        (@arg print_type: -p --print +takes_value "[stderr|path] default: path (stderr doesn't support `emit_type: obj`)")
        (@arg O2: -O "Optimized code")
        (@arg Verbose: -V --verbose +takes_value "verbose level: 0, 1, 2")
        (@arg output: -o --output +takes_value "object file output path")
        (@arg dryrun: --dry "DRY RUN print config")

    ).get_matches();

    /* Build File && Output */
    let source_file_path = matches.value_of("FILE").unwrap();

    let input = PathBuf::from(source_file_path);
    let file = SrcFileInfo::new(input)?;

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

    /* Emit Type */
    let emit_type
    = if let Some(emit_type) = matches.value_of("emit_type") {
        match emit_type {
            "obj" => {
                EmitType::Obj
            },
            "asm" => {
                EmitType::Asm
            },
            "llvm-ir" => {
                EmitType::LLVMIR
            },
            _ => unimplemented!()
        }
    }
    else {
        EmitType::Obj
    };

    /* Build OptLv */
    let optlv
    = if matches.is_present("O2") {
        OptLv::Opt(2)
    }
    else {
        OptLv::Debug
    };

    /* Build Verbose */
    unsafe {
        VERBOSE =
        if let Some(vs) = matches.value_of("Verbose") {
            VerboseLv::from(vs.parse::<usize>()?)
        }
        else {
            VerboseLv::V0
        }
    }

    match emit_type {
        EmitType::LLVMIR => {
            /* Print Type */
            let print_type
            = if let Some(print_type) = matches.value_of("print_type") {
                match print_type {
                    "path" => {
                        let output = match matches.value_of("output") {
                            Some(output) => output,
                            None => "a.ll"
                        };
                        let objpath = PathBuf::from(output);

                        PrintTy::File(objpath)
                    },
                    "stderr" => {
                        PrintTy::StdErr
                    },
                    _ => unimplemented!()
                }
            }
            else {
                PrintTy::StdErr
            };

            let compiler_config = CompilerConfig {
                optlv,
                target_type,
                file,
                emit_type,
                print_type
            };

            // dry run point
            if matches.is_present("dryrun") {
                println!("{:#?}", compiler_config);
                return Ok(())
            }

            compile(&compiler_config)?;
        },
        EmitType::Obj => {
            match target_type {
                TargetType::Bin => {
                    let mut tmp_out_fn = String::from("__barelang_output");
                    tmp_out_fn.push_str(&unique_suffix());
                    let objpath = PathBuf::from(tmp_out_fn.clone());

                    let print_type = PrintTy::File(objpath);

                    let compiler_config = CompilerConfig {
                        optlv,
                        target_type,
                        file,
                        emit_type,
                        print_type
                    };

                    // dry run point
                    if matches.is_present("dryrun") {
                        println!("{:#?}", compiler_config);
                        return Ok(())
                    }

                    compile(&compiler_config)?;

                    let output = match matches.value_of("output") {
                        Some(output) => output,
                        None => "a.out"
                    };

                    link(output, &[&tmp_out_fn])?;

                    fs::remove_file(tmp_out_fn)?;
                },
                _ => todo!()
            }

        },
        EmitType::Asm => todo!()
    }


    Ok(())
}


/// Compile source code string
pub fn compile(config: &CompilerConfig) -> Result<(), Box<dyn Error>> {
    let tokens = tokenize(&config.file);
    let tokens = tokens
    .into_iter()
    .filter(|tok| tok.name() != "<sp>" && !tok.name().contains("comment"))
    .collect();

    let mut parser = Parser::new(tokens);
    let ml = parser.parse()?;

    if unsafe { VERBOSE >= VerboseLv::V2 } {
        println!("ML:\n{:#?}", ml);
    }

    let mlslf = MLSimplifier::new(ml);
    let bin = mlslf.simplify().unwrap();
    if unsafe { VERBOSE >= VerboseLv::V2 } {
        println!("BaBin:\n{:#?}", bin);
    }

    codegen(&config, bin)?;
    Ok(())
}
