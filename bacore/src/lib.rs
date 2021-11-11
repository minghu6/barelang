#![feature(box_syntax)]
#![feature(type_alias_impl_trait)]

#[allow(unused_imports)]
#[allow(unused_import_braces)]


pub(crate) mod core_syntax;
pub(crate) mod parse_phase_2;
pub mod error;


use std::path::{Path, PathBuf};
use std::error::Error;


use core_syntax::a_ns::NS;
use core_syntax::spec_etc::{ name_to_path};
use inkwell::context::Context;
use lisparser::data::{LispModule, ListData, SymData};
use lisparser::parser::*;

use core_syntax::CompileContext;

pub use proc_macros::{
    make_simple_error_rules,
    //ht
};




/// Load Into Language Core
pub fn load_core() -> Result<(), Box<dyn Error>> {
    let vmctx = inkwell::context::Context::create();
    let mut ctx = CompileContext::new("core", &vmctx);

    let core_dir = Path::new("./core");

    let ns: NS = NS::try_from(core_dir)?;
    ns.load(&mut ctx)?;

    Ok(())
}

