#![allow(unused_imports)]
#![allow(unused_import_braces)]

extern crate proc_macro;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::{Paren, Priv};
use syn::{
    parse_macro_input, Expr, ExprMethodCall, ExprPath, Ident, Lit, LitInt,
    LitStr, MacroDelimiter, Path, Token,
};

////////////////////////////////////////////////////////////////////////////////
//// VecMacroRules

struct VecMacroRules {
    name: Ident,
    path: Path,
    inc_op: Ident,
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

        Ok(Self { name, path, inc_op })
    }
}

#[proc_macro]
pub fn make_vec_macro_rules(input: TokenStream) -> TokenStream {
    let VecMacroRules { name, path, inc_op } =
        parse_macro_input!(input as VecMacroRules);

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
    rules: Vec<(Ident, LitStr, Ident)>,
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
    let MakeCharMatcherRules { rules } =
        parse_macro_input!(input as MakeCharMatcherRules);

    let mut token_stream = quote! {};

    for (name, patstr, matcher_t) in rules {
        let matcher_fn_name = Ident::new(
            &format!("{}_m", name.to_string().to_lowercase()),
            Span::call_site(),
        );
        let matcher_reg_name = Ident::new(
            &format!("{}_REG", name.to_string().to_uppercase()),
            Span::call_site(),
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
        } else {
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
    rules: Vec<(Ident, LitStr)>,
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
    let MakeTokenMatcherRules { rules } =
        parse_macro_input!(input as MakeTokenMatcherRules);

    let mut token_stream = quote! {};

    for (name, patstr) in rules {
        let matcher_fn_name = Ident::new(
            &format!("{}_tok_m", name.to_string().to_lowercase()),
            Span::call_site(),
        );
        let matcher_reg_name = Ident::new(
            &format!("{}_TOK_REG", name.to_string().to_uppercase()),
            Span::call_site(),
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
    name: Ident,
}

impl Parse for MakeSimpleError {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;

        Ok(Self { name })
    }
}

#[proc_macro]
pub fn make_simple_error_rules(input: TokenStream) -> TokenStream {
    let MakeSimpleError { name } =
        parse_macro_input!(input as MakeSimpleError);

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
            pub fn new_box_err(msg: &str) -> Box<dyn std::error::Error> {
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
//// Load VM Common Type
struct LoadVMCommonType {
    ctx: Expr,
}

impl Parse for LoadVMCommonType {
    fn parse(input: ParseStream) -> Result<Self> {
        let context_name = input.parse()?;

        Ok(Self { ctx: context_name })
    }
}

#[proc_macro]
pub fn load_vm_common_ty(input: TokenStream) -> TokenStream {
    let LoadVMCommonType { ctx } =
        parse_macro_input!(input as LoadVMCommonType);

    TokenStream::from(quote! {
        use inkwell::AddressSpace;

        // Int type
        let i8_t = #ctx.i8_type();
        let i32_t = #ctx.i32_type();
        let i64_t = #ctx.i64_type();
        let i128_t = #ctx.i128_type();

        #[cfg(target_pointer_width = "64")]
        let size_t = #ctx.i64_type();

        #[cfg(target_pointer_width = "32")]
        let size_t = #ctx.i32_type();

        let sizeptr_t = size_t.ptr_type(AddressSpace::Generic);

        // Ret Void Type
        let void_t = #ctx.void_type();

        // Ptr Type
        let i8ptr_t = i8_t.ptr_type(AddressSpace::Generic);
        let i32ptr_t = i32_t.ptr_type(AddressSpace::Generic);
        let i64ptr_t = i64_t.ptr_type(AddressSpace::Generic);
        let i128ptr_t = i128_t.ptr_type(AddressSpace::Generic);

        // Float Type
        let f64_t = #ctx.f64_type();

    })
}



////////////////////////////////////////////////////////////////////////////////
//// Add VM Function Header (External)


struct VMPriTy {
    name: Ident,
    ptrlv: LitInt,
}

impl Parse for VMPriTy {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ptrlv = 0;
        while input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            ptrlv += 1;
        }

        let name = input.parse::<Ident>()?;
        let ptrlv = LitInt::new(&ptrlv.to_string(), Span::call_site());

        Ok(Self { name, ptrlv })
    }
}


struct FunHdr {
    name: Ident,
    args: Punctuated<VMPriTy, Token![,]>,
    ret: VMPriTy,
}

impl Parse for FunHdr {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;

        let args;
        parenthesized!(args in input);

        let args = args.parse_terminated(VMPriTy::parse)?;

        input.parse::<Token![->]>()?;

        let ret = input.parse::<VMPriTy>()?;

        Ok(Self { name, args, ret })
    }
}


struct ImplFunHdr {
    module: Ident,
    funhdrs: Punctuated<FunHdr, Token![;]>,
}

impl Parse for ImplFunHdr {
    fn parse(input: ParseStream) -> Result<Self> {
        let module = input.parse::<Ident>()?;
        input.parse::<Token![|]>()?;

        let funhdrs = input.parse_terminated(FunHdr::parse)?;

        Ok(Self {
            module,
            funhdrs,
        })
    }
}

/// Cooperate with load_vm_common_ty
#[proc_macro]
pub fn impl_fn_hdr(input: TokenStream) -> TokenStream {
    let ImplFunHdr {
        module,
        funhdrs,
    } = parse_macro_input!(input as ImplFunHdr);

    let mut ts = quote! {
    };

    for funhdr in funhdrs {
        let fname = funhdr.name;
        let ret = funhdr.ret;
        let ret_name = ret.name;
        let ret_ptrlv = ret.ptrlv;

        /* I have to repeate myself for quote! return private struct
         (its ret type can't exposed to function args or ret).
         */
        let ty = ret_name;
        let ptrlv = ret_ptrlv.base10_parse::<u16>().unwrap();

        let mut ty_ts = match ty.to_string().as_str() {
            "i8" | "u8" => quote! {
                i8_t
            },
            "usize" => quote! {
                size_t
            },
            "void" => quote! {
                void_t
            },
            _ => panic!("Unsupported vm type name"),
        };
        for _ in 0..ptrlv {
            ty_ts.extend(quote! {
                .ptr_type(AddressSpace::Generic)
            })
        }

        let mut args_ts = quote! {
        };

        for arg in funhdr.args {
            let ty = arg.name;
            let ptrlv = arg.ptrlv.base10_parse::<u16>().unwrap();

            let mut ty_ts = match ty.to_string().as_str() {
                "i8" | "u8" => quote! {
                    i8_t
                },
                "usize" => quote! {
                    size_t
                },
                "void" => quote! {
                    void_t
                },
                _ => panic!("Unsupported vm type name"),
            };
            for _ in 0..ptrlv {
                ty_ts.extend(quote! {
                    .ptr_type(AddressSpace::Generic)
                })
            }
            ty_ts.extend(quote! {
                .into(),
            });

            args_ts.extend(ty_ts);
        }


        ts.extend(quote! {
            #module.add_function(
                stringify!(#fname),
                #ty_ts
                    .fn_type(&[#args_ts], false),
                Some(Linkage::External)
            );
        });
    }

    TokenStream::from(ts)
}


////////////////////////////////////////////////////////////////////////////////
/////// ht![Head | Tail] -> New Vector

// struct HT {
//     head: Priv,
//     tail: Priv
// }

// impl Parse for HT {
//     fn parse(input: ParseStream) -> Result<Self> {
//         if input.peek2(Paren) {
//             input.parse::<Expr>(parser)
//         }


//         let head = input.parse()?;
//         input.parse::<Token!(|)>()?;
//         let tail = input.parse()?;

//         Ok(Self {
//             head, tail
//         })
//     }
// }

// #[proc_macro]
// pub fn ht(input: TokenStream) -> TokenStream {
//     let HT {
//         head,
//         tail
//     } = parse_macro_input!(input as HT);

//     let new_vec_name = Ident::new(
//         &Uuid::new_v4().to_simple().to_string(),
//         Span::call_site()
//     );

//     TokenStream::from(quote! {
//         {
//             let mut #new_vec_name = vec![#head];
//             #new_vec_name.extend(#tail.iter());

//             #new_vec_name
//         }
//     })
// }



#[cfg(test)]
mod tests {}
