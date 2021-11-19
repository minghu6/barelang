#![feature(type_alias_impl_trait)]


pub use proc_macros::{
    make_simple_error_rules,
    //ht
};

pub mod config;
pub mod env;
pub mod linker;
pub mod lexer;
pub mod runner;
pub mod target_generator;
pub mod error;
pub mod vmbuilder;
pub mod etc_utils;

