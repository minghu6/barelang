use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct BaCErr {
    msg: String
}

impl fmt::Display for BaCErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for BaCErr {}


impl BaCErr {
    pub fn new(msg: &str) -> Box<dyn Error> {
        Box::new(Self {
            msg: msg.to_string()
        })
    }
}
