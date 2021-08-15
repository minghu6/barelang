#![feature(extend_one)]
#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(impl_trait_in_bindings)]

#![allow(mixed_script_confusables)]
#![allow(incomplete_features)]

pub mod gram;
pub mod rules;
pub mod dsl;
pub mod lexer;
pub mod synax_parser;
pub mod semantic_analyzer;
pub mod baredata;
pub mod compiler;
pub mod codegen;
pub mod utils;


extern crate proc_macros; /* to avoid a cargo bug when cross-compiling (e.g. wasm) */

extern crate lazy_static;

pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules
};

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);