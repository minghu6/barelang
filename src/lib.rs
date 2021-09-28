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
pub mod ml_simplifier;
pub mod manual_parser;
pub mod datalsp;
pub mod datair;
pub mod datacg;
pub mod codegen;
pub mod rsclib;
pub mod utils;
pub mod error;
pub mod dbi;


use lexer::SrcFileInfo;
pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules,
    make_simple_error_rules,
    //ht
};
use utils::PrintTy;

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);

pub static mut VERBOSE: VerboseLv = VerboseLv::V0;

/// check if current verbose level meet the required verbose lv
#[inline]
pub fn verbose_enable(verbose: VerboseLv) -> bool {
    unsafe {
        VERBOSE >= verbose
    }
}

#[inline]
pub fn verbose_enable_v2() -> bool {
    verbose_enable(VerboseLv::V2)
}

#[inline]
pub fn verbose_enable_v1() -> bool {
    verbose_enable(VerboseLv::V1)
}


///////////////////////////////////////////////////////////////////////////
//// Compiler Config

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLv {
    Debug,
    Opt(usize)
}

#[derive(Debug, PartialEq, Eq)]
pub enum TargetType {
    Bin,
    ReLoc,
    DyLib,
}


#[derive(Debug, PartialEq, Eq)]
pub enum EmitType {
    LLVMIR,
    Asm,
    Obj
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VerboseLv {
    V0,
    V1,
    V2
}

impl From<usize> for VerboseLv {
    fn from(i: usize) -> Self {
        match i {
            0 => Self::V0,
            1 => Self::V1,
            2 => Self::V2,
            _ => Self::V0
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
pub struct CompilerConfig {
    pub optlv: OptLv,
    pub target_type: TargetType,
    pub emit_type: EmitType,
    pub file: SrcFileInfo,
    pub print_type: PrintTy
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


#[cfg(test)]
mod test {

    #[test]
    fn test_ht() {
        use crate::ht;

        let a = 1;
        let c = 3;
        let b = vec![2];

        println!("{:?}", ht![a | b]);

        println!("{:?}", ht![c | ht![a| ht![a | b]]]);

        println!("{:?}", ht![2|b]);

        println!("{:?}", ht![2| vec![3] ]);

        println!("{:?}", ht![0 | ht![1 | ht![2| ht![3]]]]);

    }
}
