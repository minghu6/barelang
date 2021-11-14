use std::{error::Error, process::{Command, Stdio}};

use crate::{env::{bare_home, librsc_path}, runner::RunningError};




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
    .arg(librsc_path().to_str().unwrap())
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

