#![feature(extend_one)]
#![feature(type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(let_chains)]
#![feature(option_get_or_insert_default)]

#![allow(mixed_script_confusables)]
#![allow(incomplete_features)]

pub mod gram;
pub mod rules;
pub mod dsl;
pub mod lexer;
pub mod syntax_parser;
pub mod semantic_analyzer;
pub mod ml_simplifier;
pub mod datalsp;
pub mod datair;
pub mod codegen;
pub mod rsclib;
pub mod utils;
pub mod error;
pub mod dbi;

use std::path::PathBuf;

use lexer::SrcFileInfo;
pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules,
    make_simple_error_rules
};

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);

pub static mut VERBOSE: VerboseLv = VerboseLv::V0;

///////////////////////////////////////////////////////////////////////////
//// Compiler Config

pub enum OptLv {
    Debug,
    Opt(usize)
}

pub enum TargetType {
    Bin,
    ReLoc,
    DyLib
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VerboseLv {
    V0,
    V1,
    V2
}

pub struct CompilerConfig {
    pub optlv: OptLv,
    pub target_type: TargetType,
    pub file: SrcFileInfo,
    pub objpath: PathBuf
}


impl CompilerConfig {
    pub fn get_module_name(&self) -> String {
        self.file.get_path().file_stem().unwrap().to_string_lossy().to_string()
    }

    pub fn filename(&self) -> String {
        self.file.get_path().file_name().unwrap().to_string_lossy().to_string()
    }

    pub fn dirname(&self) -> String {
        self.file.get_path().parent().unwrap().to_string_lossy().to_string()
    }
}
