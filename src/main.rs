use std::error::Error;
use std::fs;
use std::path::{PathBuf, Path};
use std::time::{SystemTime, UNIX_EPOCH};

use bac::middleware::ml_simplifier::MLSimplifier;
use bacommon::config::{
    CompilerConfig, EmitType, OptLv, PrintTy, TargetType, VerboseLv,
};
use bacommon::env::libcore_path;
use bacommon::error::*;
use bacommon::lexer::SrcFileInfo;
use bacommon::linker::link;
use bacommon::r#const::*;

use bacore::VMCtxHolder;
use clap::clap_app;
use clap::{ArgMatches};

use bac::backend::codegen::codegen;
use bac::frontend::lexer::tokenize;
use bac::frontend::manual_parser::Parser;

use bac::*;

fn unique_suffix() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
        .to_string()
}


pub enum SrcType {
    BareLang,
    BaCore,
}

impl SrcType {
    pub fn try_new(path: &Path) -> Result<Self, Box<dyn Error>> {
        let filename_s = path.file_name().unwrap().to_str().unwrap();

        Ok(
            if filename_s.ends_with(EXT_BARE_CORE)
                && filename_s.len() > EXT_BARE_CORE.len()
            {
                Self::BaCore
            } else if filename_s.ends_with(EXT_BARE_LANG)
                && filename_s.len() > EXT_BARE_LANG.len()
            {
                Self::BareLang
            } else {
                return Err(UnknownFileExtNameError::new_box_err(filename_s));
            }
        )
    }
}




fn handle_compile(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    /* Build File && Output */
    let source_file_path = matches.value_of("FILE").unwrap();

    let input = PathBuf::from(source_file_path);
    let file = SrcFileInfo::new(input)?;

    /* Build Target Type */
    let target_type =
        if let Some(target_type) = matches.value_of("target_type") {
            match target_type {
                "bin" => TargetType::Bin,
                _ => unimplemented!(),
            }
        } else {
            TargetType::Bin
        };

    /* Emit Type */
    let emit_type = if let Some(emit_type) = matches.value_of("emit_type") {
        match emit_type {
            "obj" => EmitType::Obj,
            "asm" => EmitType::Asm,
            "llvm-ir" => EmitType::LLVMIR,
            _ => unimplemented!(),
        }
    } else {
        EmitType::Obj
    };

    /* Build OptLv */
    let optlv = if matches.is_present("O2") {
        OptLv::Opt(2)
    } else {
        OptLv::Debug
    };

    /* Build Verbose */
    unsafe {
        VERBOSE = if let Some(vs) = matches.value_of("Verbose") {
            VerboseLv::from(vs.parse::<usize>()?)
        } else {
            VerboseLv::V0
        }
    }

    match emit_type {
        EmitType::LLVMIR => {
            /* Print Type */
            let print_type =
                if let Some(print_type) = matches.value_of("print_type") {
                    match print_type {
                        "path" => {
                            let output = match matches.value_of("output") {
                                Some(output) => output,
                                None => "a.ll",
                            };
                            let objpath = PathBuf::from(output);

                            PrintTy::File(objpath)
                        }
                        "stderr" => PrintTy::StdErr,
                        _ => unimplemented!(),
                    }
                } else {
                    PrintTy::StdErr
                };

            let compiler_config = CompilerConfig {
                optlv,
                target_type,
                emit_type,
                print_type,
            };

            // dry run point
            if matches.is_present("dryrun") {
                println!("{:#?}", compiler_config);
                return Ok(());
            }

            compile(compiler_config, file)?;
        }
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
                        emit_type,
                        print_type,
                    };

                    // dry run point
                    if matches.is_present("dryrun") {
                        println!("{:#?}", compiler_config);
                        return Ok(());
                    }

                    compile(compiler_config, file)?;

                    let output = match matches.value_of("output") {
                        Some(output) => output,
                        None => "a.out",
                    };

                    link(output, &[&tmp_out_fn])?;

                    fs::remove_file(tmp_out_fn)?;
                }
                _ => todo!(),
            }
        }
        EmitType::Asm => todo!(),
    }



    Ok(())
}


fn handle_precompile(matches: &ArgMatches) -> Result<(), Box<dyn Error>> {
    /* Build Target Type */
    let target_type = TargetType::DyLib;

    /* Emit Type */
    let emit_type = if let Some(emit_type) = matches.value_of("emit_type") {
        dbg!(emit_type);
        match emit_type {
            "obj" => EmitType::Obj,
            "llvm-ir" => EmitType::LLVMIR,
            _ => return Err(XXXError::new_box_err(&emit_type.to_string())),
        }
    } else {
        EmitType::Obj
    };

    /* Build OptLv */
    let optlv = if matches.is_present("O2") {
        OptLv::Opt(2)
    } else {
        OptLv::Debug
    };

    /* Build Verbose */
    unsafe {
        VERBOSE = if let Some(vs) = matches.value_of("Verbose") {
            VerboseLv::from(vs.parse::<usize>()?)
        } else {
            VerboseLv::V0
        }
    }

    /* Print Type */
    let print_type =
    if let Some(path_str) = matches.value_of("output") {
        PrintTy::File(PathBuf::from(path_str))
    } else {
        match emit_type {
            EmitType::LLVMIR => PrintTy::StdErr,
            EmitType::Obj => PrintTy::File(libcore_path()),
            _ => unimplemented!()
        }
    };

    let compiler_config = CompilerConfig {
        optlv,
        target_type,
        emit_type,
        print_type,
    };

    let holder = VMCtxHolder::new(compiler_config);

    holder.gen_core_lib()

}


/// Compile source code string
fn compile(config: CompilerConfig, file: SrcFileInfo) -> Result<(), Box<dyn Error>> {
    let dyn_compile = match SrcType::try_new(file.get_path())? {
        SrcType::BareLang => compile_barelang,
        SrcType::BaCore => compile_bacore,
    };

    dyn_compile(config, file)
}

fn compile_bacore(config: CompilerConfig, _file: SrcFileInfo)  -> Result<(), Box<dyn Error>> {

    let _holder = VMCtxHolder::new(config);



    Ok(())
}


fn compile_barelang(config: CompilerConfig, file: SrcFileInfo)  -> Result<(), Box<dyn Error>> {
    let tokens = tokenize(&file);
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

    codegen(&config, file, bin)
}


fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap_app!(bac =>
        (version: "0.1.0")
        (author: "minghu6")
        (about: "Bare Language Compiler")

        (@subcommand compile =>
            (@arg FILE: +required "input source file (exp *.ba)")
            (@arg target_type: -t --target +takes_value "[bin|dylib|reloc] default: bin")
            (@arg emit_type: -e --emit +takes_value "[llvm-ir|asm|obj] default: obj")
            (@arg print_type: -p --print +takes_value "[stderr|path] default: path (stderr doesn't support `emit_type: obj`)")
            (@arg O2: -O "Optimized code")
            (@arg Verbose: -V --verbose +takes_value "verbose level: 0, 1, 2")
            (@arg output: -o --output +takes_value "object file output path")
            (@arg dryrun: --dry "DRY RUN print config")
        )
        (@subcommand precompile =>
            (about: "compile runtime binary")
            (@arg emit_type: -e --emit +takes_value "[llvm-ir|obj] default: obj")
            (@arg output: -o --output +takes_value "output path")
            (@arg O2: -O "Optimized code")
            (@arg Verbose: -V --verbose +takes_value "verbose level: 0, 1, 2")
        )

    ).get_matches();

    if let Some(precompile_match) = matches.subcommand_matches("precompile") {
        handle_precompile(precompile_match)
    }
    else if let Some(compile_match) = matches.subcommand_matches("compile") {
        handle_compile(compile_match)
    }
    else {
        unimplemented!()
    }

}
