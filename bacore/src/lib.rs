#![feature(box_syntax)]
#![feature(type_alias_impl_trait)]


#[allow(unused_imports)]
#[allow(unused_import_braces)]


pub(crate) mod core_syntax;
pub(crate) mod parse_phase_2;


use std::marker::PhantomData;
use std::error::Error;

use bacommon::config::CompilerConfig;
use inkwell::context::Context;

use core_syntax::{a_ns::ANS, hardcode_fns::load_primitive_function};
use bacommon::env::*;


pub use proc_macros::{
    make_simple_error_rules,
    //ht
};


pub struct VMCtxHolder<'ctx> {
    vmctx: Context,
    config: CompilerConfig,
    _marker: PhantomData<&'ctx ()>
}

impl<'ctx> VMCtxHolder<'ctx> {
    pub fn new(config: CompilerConfig) -> Self {
        VMCtxHolder {
            vmctx: Context::create(),

            config,
            _marker: PhantomData
        }
    }


    /// Load Into Language Core
    pub fn load_core(&'ctx self) -> Result<ANS<'ctx>, Box<dyn Error>> {
        let core_dir = core_src_dir();

        let mut ns: ANS = ANS::init(core_dir.as_path(), &self.vmctx)?;
        load_primitive_function(&mut ns)?;
        ns.load()?;

        Ok(ns)
    }

    pub fn load_mod(&self, ) -> Result<ANS<'ctx>, Box<dyn Error>> {


        todo!()
    }

    pub fn gen_core_lib(&self) -> Result<(), Box<dyn Error>> {
        let ns = self.load_core()?;

        ns.print(&self.config)
    }

}


#[cfg(test)]
mod test {
    use std::error::Error;

    use bacommon::config::*;

    use crate::VMCtxHolder;

    #[test]
    fn test_compile_core() -> Result<(), Box<dyn Error>> {
        let compiler_config = CompilerConfig {
            optlv: OptLv::Debug,
            target_type: TargetType::DyLib,
            emit_type: EmitType::LLVMIR,
            print_type: PrintTy::StdErr,
        };

        let holder = VMCtxHolder::new(compiler_config);

        todo!();
        Ok(())
    }
}

