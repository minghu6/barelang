#![allow(unused_imports)]
use std::{error::Error, process::{Command, Stdio}};

use crate::{env::{ librsc_path, runtime_dir, libmixin_a_path, libmixin_so_path}, runner::RunningError};




/// ```none
/// Default Args:
///   * input object name: output.o
///   * input dir: .
///
/// link dylib
/// ```
pub fn link_default(output: &str) -> Result<(), Box<dyn Error>> {
    link(output, &["output.o"])
}


pub fn link(output: &str, input_list: &[&str]) -> Result<(), Box<dyn Error>> {
    // gcc output.o libbare.so -Xlinker -rpath ./ -o main

    let mut link_proc = Command::new("gcc")
    .args(input_list)
    .arg(librsc_path().to_str().unwrap())
    // .arg("-Xlinker")
    // .arg("-rpath")
    // .arg(runtime_dir())
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


/// link libmixin.a
pub fn link2_default(output: &str) -> Result<(), Box<dyn Error>> {
    link2(output, &["output.o"])
}


pub fn link2(output: &str, input_list: &[&str]) -> Result<(), Box<dyn Error>> {
    // gcc output.o ./lib/libmixin.a -o main

    let mut link_proc = Command::new("gcc")
    .args(input_list)
    .arg(libmixin_a_path().to_str().unwrap())
    // cargo rustc -- --print native-static-libs
    .args("-lgcc_s -lutil -lrt -lpthread -lm -ldl -lc".split(" "))
    // .arg(libmixin_so_path().to_str().unwrap())
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
