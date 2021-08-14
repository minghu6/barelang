////////////////////////////////////////////////////////////////////////////////
//// Grammar DSL
//// 效果差强人意，代价大概是
//// ```
//// #![allow(non_snake_case)]
//// #![allow(unused_variables)]
////```


pub struct Sym {}

#[macro_export]
macro_rules! declare_terminal {
    ($name:ident) => {
        let $name = $crate::gram::GramSym::Terminal(
            stringify!($name).to_string()
        );
    };

    ( $($name:ident),* ) => {
        $(
            declare_terminal!{$name};
        )+
    };
}


/// 暂时什么都不做
#[macro_export]
macro_rules! declare_nonterminal {
    ($name:ident) => {
        let $name = $crate::gram::GramSym::NonTerminal(
            stringify!($name).to_string()
        );
    };

    ( $($name:ident),* ) => {
        $(
            declare_nonterminal!($name);
        )+
    }
}

//  限制只能引入已存在的符号
#[macro_export]
macro_rules! use_epsilon {
    ($name:ident) => {
        debug_assert!(stringify!($name) == "ε", "epsilon sym just should be `ε`");
        declare_nonterminal!($name);
    };
}

/// 创建一个规则
/// 第一个产生式默认是入口的根语法
#[macro_export]
macro_rules! grammar {
    [$gram_name:ident|
         $($name:ident : $(| $($gramsym:ident)+ ;)+ )+
    |] =>
    {
        {
            let mut _grammar = $crate::gram::Gram::new(stringify!($gram_name));
            $(
                $(
                    let mut gram_str_vec = Vec::<$crate::gram::GramSym>::new();
                    let mut has_epsilon = false;

                    $(
                        if stringify!($gramsym) == "ε" {
                            has_epsilon = true;
                        } else {
                            gram_str_vec.push($gramsym.clone());
                        }
                    )+

                    let gramsymstr = if has_epsilon {
                        $crate::gram::GramSymStr::Epsilon
                     } else {
                        $crate::gram::GramSymStr::Str(gram_str_vec)
                     };

                    _grammar.insert_prod(
                        ($name.clone(), gramsymstr)
                    );
                )+
            )+

            _grammar
        }
    }
}


#[macro_export]
macro_rules! First {
    ($name:ident | $($sym:ident)+) => {
        {
            use indexmap::indexset;

            let mut _set = indexset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "ε" {
                    $crate::gram::FstSetSym::Epsilon
                } else {
                    $crate::gram::FstSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::gram::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}

#[macro_export]
macro_rules! Follow {
    ($name:ident | $($sym:ident)+) => {
        {
            use indexmap::indexset;

            let mut _set = indexset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "NUL" {
                    $crate::gram::FollSetSym::EndMarker
                } else {
                    $crate::gram::FollSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::gram::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Lex DFA Map

#[macro_export]
macro_rules! lexdfamap {
    ( $($cur_state:expr =>
        {
            $( $matcher:ident | $nxt_state:expr, $flag:literal )*
        }
      ),*
    ) => {
        {
            let mut _vec = Vec::new();

            $(
                let mut trans_vec = Vec::new();

                $(
                    trans_vec.push((
                        $matcher as fn(&char) -> bool,
                        ($nxt_state, $flag)
                    ));
                )*

                _vec.push(($cur_state, trans_vec));
            )*

            _vec
        }
    }
}

#[macro_export]
macro_rules! token_analyzer {
    ( $($token_name:ident => $patstr:literal),* ) => {
        {
            let mut _vec = vec![];

            $(
                _vec.push((
                    RegexTokenMatcher::new($patstr),
                    stringify!($token_name).to_string()
                ));
            )*

            _vec
        }
    }
}
