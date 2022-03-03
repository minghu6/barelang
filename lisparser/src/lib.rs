#![feature(box_syntax)]
#![feature(fmt_internals)]


pub mod parser;
pub mod data;
pub mod error;
pub mod pretty_printer;


#[macro_export]
macro_rules! sym {
    ($name:expr) => {
        {
            use $crate::data::*;
            use $crate::parser::*;

            SymData {
                val: String::from($name),
                loc: SrcLoc::default()
            }
        }
    }
}


#[macro_export]
macro_rules! key {
    ($name:expr) => {
        {
            use $crate::data::*;
            use $crate::parser::*;

            KeyData {
                val: String::from($name),
                loc: SrcLoc::default()
            }
        }
    }
}


#[macro_export]
macro_rules! list {
    ($head:ident, $tail:ident) => {
        {
            use $crate::data::*;
            use $crate::parser::SrcLoc;

            ListData::NonNil(NonNilListData {
                head: box $head.try_into().unwrap(),
                tail: box $tail,
                loc: SrcLoc::default()
            })
        }
    };

    ($head_expr:expr, $tail:ident) => {
        {
            let head = $head_expr;

            list!(head, $tail)
        }
    };

    ($head_expr:expr, $tail_expr:expr) => {
        {
            let head = $head_expr;
            let tail = $tail_expr;

            list!(head, tail)
        }
    };

}


#[macro_export]
macro_rules! nil {
    () => {
        {
            use $crate::data::*;

            ListData::nil()
        }
    }
}

#[macro_export]
macro_rules! brace_map {
    ($($k:expr => $v:expr),*) => {
        {
            use $crate::data::*;
            use $crate::parser::SrcLoc;

            let mut entries = Vec::new();

            $(
                entries.push((
                    $k.try_into().unwrap(),
                    $v.try_into().unwrap()
                ));
            )*

            BraceMapData {
                entries,
                loc: SrcLoc::default()
            }

        }
    };
}



#[cfg(test)]
mod test {

    #[test]
    fn test_macro() {
        use crate::*;

        let sym = sym!( "do-some-thing" );
        println!("{:#?}", sym);


        let list = list!(sym!("aaa"), list!( sym!("do-some"), nil!()));
        println!("{:#?}", list);


        let brace_map = brace_map!(
            sym!("type-dd") => sym!("T"),
            sym => sym!("T")
        );
        println!("{:#?}", brace_map);

    }
}

