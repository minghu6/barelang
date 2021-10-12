#![feature(extend_one)]
#![feature(type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(let_chains)]
#![feature(option_get_or_insert_default)]
#![feature(path_file_prefix)]

#![allow(mixed_script_confusables)]
#![allow(incomplete_features)]

pub mod frontend;
pub mod middleware;
pub mod backend;

pub mod utils;
pub mod error;
pub mod rsclib;
pub mod dbi;


use std::path::{
    Path, PathBuf
};
use std::env;
use std::fs;
use std::process::{Command, Stdio};
use std::error::Error;

use frontend::lexer::SrcFileInfo;
pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules,
    make_simple_error_rules,
    //ht
};
use utils::{ PrintTy, RunningError };

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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

/// BareLang Installed Home
pub fn bare_home() -> PathBuf {
    let bare_home_str
    = env::var("BARE_HOME").unwrap_or(".".to_string());

    fs::canonicalize(Path::new(
        &bare_home_str
    )).unwrap()
}

/// Full path of librsc.so
pub fn lib_path() -> PathBuf {
    bare_home().join("librsc.so")
}


/// ```none
/// Default Args:
///   * input object name: output.o
///   * input dir: .
///
/// ```
pub fn link_default(output: &str) -> Result<(), Box<dyn Error>> {
    link(output, &["output.o"])
}


pub fn link(output: &str, input_list: &[&str]) -> Result<(), Box<dyn Error>> {
    // gcc output.o libbare.so -Xlinker -rpath ./ -o main

    let mut link_proc = Command::new("gcc")
    .args(input_list)
    .arg(lib_path().to_str().unwrap())
    .arg("-Xlinker")
    .arg("-rpath")
    .arg(bare_home().as_os_str().to_str().unwrap())
    .arg("-o")
    .arg(output)
    .stdin(Stdio::null())
    .stdout(Stdio::inherit())
    .spawn()?;

    let exit_st = link_proc.wait()?;
    if exit_st.success() {
        Ok(())
    }
    else {
        Err(RunningError::as_box_err(exit_st.code().unwrap()))
    }
}


pub fn run_bin(output: &str) -> Result<(), Box<dyn Error>> {
    // Run Test
    let exit_st = Command::new(&format!("./{}", output))
    .stdin(Stdio::null())
    .stdout(Stdio::inherit())
    .status()?;

    if exit_st.success() {
        Ok(())
    }
    else {
        Err(RunningError::as_box_err(
            match exit_st.code() {
                Some(code) => code,
                None => -1
            }
        ))
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
