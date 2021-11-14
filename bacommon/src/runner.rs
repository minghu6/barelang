use std::{error::Error, fmt, process::{Command, Stdio}};



////////////////////////////////////////////////////////////////////////////////
//// Running Error

#[derive(Debug)]
pub struct RunningError {
    code: i32
}

impl Error for RunningError {
}

impl fmt::Display for RunningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code)
    }
}

impl RunningError {
    pub fn as_box_err(code: i32) -> Box<dyn Error> {
        Box::new(Self { code })
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
    } else {
        Err(RunningError::as_box_err(match exit_st.code() {
            Some(code) => code,
            None => -1,
        }))
    }
}
