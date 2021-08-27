#![feature(extend_one)]
#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(impl_trait_in_bindings)]
#![feature(let_chains)]

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
pub mod rslib;
pub mod utils;
pub mod error;

use std::path::PathBuf;

use lexer::SrcFileInfo;
pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules
};

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);


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

pub struct CompilerConfig {
    pub optlv: OptLv,
    pub target_type: TargetType,
    pub file: SrcFileInfo,
    pub objpath: PathBuf
}


impl CompilerConfig {
    pub fn get_module_name(&self) -> String {
        self.file.get_path().file_name().unwrap().to_string_lossy().to_string()
    }
}
