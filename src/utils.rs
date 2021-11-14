

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



/// ```none
/// string unescape:
///   | \n -> 10  0x0a
///   | \r -> 13  0x0d
///   | \t -> 9   0x09
///   | \" -> 34  0x22
///   | \\ -> 92  0x5c
///   | \' -> 39  0x27
/// ```
pub fn unescape_str(escaped_str: &str) -> Result<String, char> {
    enum State {
        Normal,
        EscapeReady,
    }

    enum Strategy {
        PushPop,
        JustPush,
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
            (State::EscapeReady, 'n' | 'r' | 't' | '"' | '\\' | '\'') => {
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
            Strategy::UnEscape => {
                output.push(
                    match u {
                        'n'  => '\u{000a}',
                        'r'  => '\u{000d}',
                        't'  => '\u{0009}',
                        '"'  => '\u{0022}',
                        '\\' => '\u{005c}',
                        '\'' => '\u{0027}',
                        _ => unreachable!()
                    }
                );
            }
        };
    }

    Ok(output)
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
