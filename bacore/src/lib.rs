#[allow(unused_imports)]
#[allow(unused_import_braces)]


pub(crate) mod core_syntax;
pub mod error;


use std::path::Path;
use std::error::Error;

use lisparser::data::{IdData, LispModule, ListData};
use lisparser::parser::*;

pub use proc_macros::{
    make_simple_error_rules,
    //ht
};


/// Load Into Language Core
pub fn load_core() -> Result<(), Box<dyn Error>> {
    let core_path = Path::new("./core");
    let mod_fn = Path::new("mod.bacore.lisp");

    let core_mod_path= core_path.join(mod_fn);

    load_file(&core_mod_path)?;


    Ok(())
}


pub fn load_file(path: &Path) -> Result<(), Box<dyn Error>> {
    let mut parser = LispParser::new(path)?;
    let lm = parser.parse()?;

    Ok(())
}

fn analyze(lispmodule: LispModule) -> Result<(), Box<dyn Error>> {
    for paren_list in lispmodule.lists {
        if let ListData::NonNil(nonnil) = paren_list {
            let id_data: IdData = (*nonnil.head).try_into()?;


        }
    }

    Ok(())
}


