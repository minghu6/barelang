use std::path::PathBuf;
use std::error::Error;
use std::fmt::Display;


pub type CounterType = impl FnMut() -> usize;

pub fn gen_counter() -> CounterType {
    gen_counter_1(0)
}

pub fn gen_counter_1(init: usize) -> CounterType {
    let mut count = init;

    move || {
        let old_count = count;
        count += 1;
        old_count
    }
}


#[derive(Debug, PartialEq, Eq)]
pub enum PrintTy {
    StdErr,
    File(PathBuf)
}

impl PrintTy {
    pub fn get_path(&self) -> Option<PathBuf> {
        if let Self::File(ref path) = self {
            Some(path.clone())
        }
        else {
            None
        }
    }
}


pub fn usize_len() -> usize {
    if cfg!(target_pointer_width="64") { 8 }
    else { 4 }
}


/// ```none
/// string unescape:
///   | \n -> 10
///   | \r -> 13
///   | \t -> 9
/// ```
pub fn unescape_str(escaped_str: &str) -> Result<String, char> {
    enum State {
        Normal,
        EscapeReady,
    }

    enum Strategy {
        PushPop,
        JustPush,
        JustDoublePop,
        UnEscape
    }

    let mut st = State::Normal;
    let mut output  = String::new();
    let mut strategy;
    // phantom cache

    for u in escaped_str.chars() {
        (st, strategy) = match (st, u) {
            (State::Normal, '\\') => {
                (State::EscapeReady, Strategy::JustPush)
            },
            (State::Normal, _) => {
                (State::Normal, Strategy::PushPop)
            },
            (State::EscapeReady, '\\') => {
                (State::Normal, Strategy::JustDoublePop)
            },
            (State::EscapeReady, 'n' | 'r' | 't') => {
                (State::Normal, Strategy::UnEscape)
            },
            (State::EscapeReady, _) => {
                return Err(u)
            },
        };

        match strategy {
            Strategy::PushPop => {
                output.push(u);
            },
            Strategy::JustPush => {
            },
            Strategy::JustDoublePop => {
                output.push('\\');
            },
            Strategy::UnEscape => {
                output.push(
                    match u {
                        'n' => '\u{000a}',
                        'r' => '\u{000d}',
                        't' => '\u{0009}',
                        _ => unreachable!()
                    }
                );
            }
        };
    }

    Ok(output)
}


////////////////////////////////////////////////////////////////////////////////
//// Running Error

#[derive(Debug)]
pub struct RunningError {
    code: i32
}

impl Error for RunningError {
}

impl Display for RunningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code)
    }
}

impl RunningError {
    pub fn as_box_err(code: i32) -> Box<dyn Error> {
        Box::new(Self { code })
    }
}


/// (literal | expr | ident): 3x3
#[macro_export]
macro_rules! ht {
    ( $head_lit:literal | $tail_lit:expr ) => {
        {
            let mut _vec = vec![$head_lit];
            _vec.extend($tail_lit.iter().cloned());
            _vec
        }
    };
    ( $head_lit:literal | $tail:ident ) => {
        {
            let mut _vec = vec![$head_lit];
            _vec.extend($tail.iter().cloned());
            _vec
        }
    };
    ( $head:ident | $tail_lit:literal ) => {
        {
            let tail = $tail_lit;
            $ht![$head | $tail]
        }
    };
    ( $head:ident | $tail:ident ) => {
        {
            let mut _vec = vec![$head];
            _vec.extend($tail.iter().cloned());
            _vec
        }
    };
    ( $head:ident | $tail:expr ) => {
        {
            let mut _vec = vec![$head];
            _vec.extend($tail.iter().cloned());
            _vec
        }
    };
    ( $head:expr, | $tail:expr ) => {
        {
            let mut _vec = vec![$head];
            _vec.extend($tail.iter().cloned());
            _vec
        }
    };
    ( $head: ident) => {
        {
            vec![$head]
        }
    };
    ( $head: expr) => {
        {
            vec![$head]
        }
    };
    ( $head: ident) => {
        {
            vec![$head]
        }
    }

}



#[cfg(test)]
mod test {
    use crate::*;

    use super::unescape_str;

    #[test]
    fn test_vec_like_macro_rules() {
        let queue = vecdeq![1, 2];

        println!("{:?}", queue);
    }

    #[test]
    fn test_unescape_str() {
        let s0 = r"abc\ndef";

        assert_eq!(s0, "abc\\ndef");
        assert_eq!(unescape_str(&s0).unwrap(), "abc\ndef");
    }
}
