extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{ Span };
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, Token, parse_macro_input, Path, LitStr};
use quote::{ quote };


////////////////////////////////////////////////////////////////////////////////
//// VecMacroRules

struct VecMacroRules {
    name: Ident,
    path: Path,
    inc_op: Ident
}

impl Parse for VecMacroRules {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let path: Path = input.parse()?;

        let inc_op;
        if let Ok(_) = input.parse::<Token![,]>() {
            inc_op = input.parse()?;
        } else {
            inc_op = Ident::new("insert", Span::call_site());
        }

        Ok(Self {
            name,
            path,
            inc_op
        })
    }
}

#[proc_macro]
pub fn make_vec_macro_rules(input: TokenStream) -> TokenStream {
    let VecMacroRules {
        name,
        path,
        inc_op
    } = parse_macro_input!(input as VecMacroRules);

    TokenStream::from(quote! {
        #[macro_export]
        macro_rules! #name {
            ( $($value:expr),* ) => {
                {
                    let mut vec_like = #path::new();

                    $(
                        vec_like.#inc_op($value);
                    )*

                    vec_like
                }
            };
        }
    })
}


////////////////////////////////////////////////////////////////////////////////
//// MakeCharMatcher

struct MakeCharMatcherRules {
    // ident, patstr, matcher_t
    rules: Vec<(Ident, LitStr, Ident)>
}

impl Parse for MakeCharMatcherRules {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = vec![];

        while !input.is_empty() {
            let name = input.parse()?;
            input.parse::<Token!(=>)>()?;
            let patstr = input.parse()?;
            input.parse::<Token!(|)>()?;
            let matcher_t = input.parse()?;

            if !input.is_empty() {
                input.parse::<Token!(,)>()?;
            }

            rules.push((name, patstr, matcher_t));
        }

        Ok(Self { rules })
    }
}

#[proc_macro]
pub fn make_char_matcher_rules(input: TokenStream) -> TokenStream {
    let MakeCharMatcherRules {
        rules
    } = parse_macro_input!(input as MakeCharMatcherRules);

    let mut token_stream = quote! {};

    for (name, patstr, matcher_t) in rules {
        let matcher_fn_name = Ident::new(
            &format!("{}_m", name.to_string().to_lowercase()),
            Span::call_site()
        );
        let matcher_reg_name = Ident::new(
            &format!("{}_REG", name.to_string().to_uppercase()),
            Span::call_site()
        );

        if matcher_t.to_string() == "r" {
            token_stream.extend(quote! {
                pub fn #matcher_fn_name(c: &char) -> bool {
                    lazy_static! {
                        static ref #matcher_reg_name: Box<dyn CharMatcher + Send + Sync>
                        = Box::new(RegexCharMatcher::new(#patstr));
                    }

                    #matcher_reg_name.is_match(c)
                }
            })
        }
        else {
            token_stream.extend(quote! {
                pub fn #matcher_fn_name(c: &char) -> bool {
                    lazy_static! {
                        static ref #matcher_reg_name: Box<dyn CharMatcher + Send + Sync>
                        = Box::new(SimpleCharMatcher::new(#patstr));
                    }

                    #matcher_reg_name.is_match(c)
                }
            })
        }
    }

    TokenStream::from(token_stream)
}

////////////////////////////////////////////////////////////////////////////////
//// MakeTokenMatcher

struct MakeTokenMatcherRules {
    // ident, patstr, matcher_t
    rules: Vec<(Ident, LitStr)>
}

impl Parse for MakeTokenMatcherRules {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = vec![];

        while !input.is_empty() {
            let name = input.parse()?;
            input.parse::<Token!(=>)>()?;
            let patstr = input.parse()?;

            if !input.is_empty() {
                input.parse::<Token!(,)>()?;
            }

            rules.push((name, patstr));
        }

        Ok(Self { rules })
    }
}

/// 只需要头部匹配就可以完成匹配, 因为之前的分词已经做了区分
#[proc_macro]
pub fn make_token_matcher_rules(input: TokenStream) -> TokenStream {
    let MakeTokenMatcherRules {
        rules
    } = parse_macro_input!(input as MakeTokenMatcherRules);

    let mut token_stream = quote! {};

    for (name, patstr) in rules {
        let matcher_fn_name = Ident::new(
            &format!("{}_tok_m", name.to_string().to_lowercase()),
            Span::call_site()
        );
        let matcher_reg_name = Ident::new(
            &format!("{}_TOK_REG", name.to_string().to_uppercase()),
            Span::call_site()
        );

        token_stream.extend(quote! {
            pub fn #matcher_fn_name(c: &str) -> bool {
                lazy_static! {
                    static ref #matcher_reg_name: Box<dyn TokenMatcher + Send + Sync>
                    = Box::new(RegexTokenMatcher::new(#patstr));
                }

                #matcher_reg_name.is_match(c)
            }
        })
    }

    TokenStream::from(token_stream)
}

////////////////////////////////////////////////////////////////////////////////
//// Define New Custom Error
struct MakeSimpleError {
    name: Ident
}

impl Parse for MakeSimpleError {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;

        Ok(Self { name })
    }
}

#[proc_macro]
pub fn make_simple_error_rules(input: TokenStream) -> TokenStream {
    let MakeSimpleError {
        name
    } = parse_macro_input!(input as MakeSimpleError);

    TokenStream::from(quote! {
        pub struct #name {
            msg: String
        }

        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.msg)
            }
        }

        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self)
            }
        }

        impl std::error::Error for #name {}

        impl #name {
            pub fn new_box_err(msg: &str) -> Box<dyn Error> {
                Box::new(Self::new(msg))
            }

            pub fn new(msg: &str) -> Self {
                Self {
                    msg: msg.to_string()
                }
            }
        }

    })
}

////////////////////////////////////////////////////////////////////////////////
/////// Invoke Matcher

// struct InvokeMatcher {
//     matcher_name: Ident
// }

// impl Parse for InvokeMatcher {
//     fn parse(input: ParseStream) -> Result<Self> {
//         if input.peek(Ident) {
//             return Ok(Self { matcher_name: input.parse()? })
//         }
//         else {
//             unreachable!()
//         }
//     }
// }

// #[proc_macro]
// pub fn invoke_matcher(input: TokenStream) -> TokenStream {
//     let InvokeMatcher {
//         matcher_name: Ident
//     } = parse_macro_input!(input as InvokeMatcher);

//     let mut token_stream = quote! {};

//     let invoker_name = Ident::new(
//         &format!("{}_m", #matcher_name.to_lowercase()),
//         Span::call_site()
//     );

//     token_stream.extend(quote! {
//         pub fn #matcher_fn_name(c: &char) -> bool {
//             lazy_static! {
//                 static ref #matcher_reg_name: Box<dyn CharMatcher + Send + Sync>
//                 = Box::new(RegexCharMatcher::new(#patstr));
//             }

//             #matcher_reg_name.is_match(c)
//         }
//     });

//     TokenStream::from(token_stream)
// }


#[cfg(test)]
mod tests {

}
