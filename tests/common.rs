#![allow(unused_attributes)]
#![feature(box_syntax)]
#![feature(path_file_prefix)]


use std::error::Error;
use std::marker::PhantomData;
use std::path::Path;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;


use inkwell::values::{IntValue, PointerValue};
use proc_macros::load_vm_common_ty;

#[macro_export]
macro_rules! module_name {
    () => {{
        let path = std::path::Path::new(file!());

        path.file_prefix().unwrap().to_str().unwrap().to_string()
    }};
}


#[allow(unused)]
pub struct VMCtxHolder<'ctx> {
    pub ctx: Context,
    _marker: PhantomData<&'ctx ()>,
}

#[allow(unused)]
impl<'ctx> VMCtxHolder<'ctx> {
    pub fn new() -> Self {
        let ctx = Context::create();

        Self {
            ctx,
            _marker: PhantomData,
        }
    }

    pub fn init_module(&'ctx self, modname: &str) -> Module<'ctx> {
        let module = self.ctx.create_module(modname);

        load_vm_common_ty!(self.ctx);

        //////////////////////////////////////////////////////////////////////
        //// POSIX Functions

        /* printf */
        let fn_printf_t = i32ptr_t.fn_type(&[i8ptr_t.into()], true);
        module.add_function(
            "printf",
            fn_printf_t,
            Some(Linkage::External),
        );

        /* open */
        let fn_open_t = i32_t.fn_type(&[i8ptr_t.into(), i32_t.into()], true);
        module.add_function(
            "open",
            fn_open_t,
            Some(Linkage::External)
        );

        /* write */
        let fn_write_t = i128_t.fn_type(&[i32_t.into(), i8ptr_t.into()], false);
        module.add_function(
            "write",
            fn_write_t,
            Some(Linkage::External)
        );

        /* close */
        let fn_close_t = i32_t.fn_type(&[i32_t.into()], false);
        module.add_function(
            "close",
            fn_close_t,
            Some(Linkage::External)
        );



        module
    }

    pub fn get_builder(&'ctx self) -> Builder<'ctx> {
        self.ctx.create_builder()
    }

    pub fn append_main(&'ctx self, module: &Module<'ctx>) -> BasicBlock<'ctx> {
        load_vm_common_ty!(self.ctx);

        let fn_main_t = i64_t.fn_type(&[], false);
        let fn_main = module.add_function("main", fn_main_t, None);

        self.ctx.append_basic_block(fn_main, "blk_main")
    }

    pub fn build_local_str(&'ctx self, builder: &Builder<'ctx>, value: &str) -> (PointerValue<'ctx>, IntValue<'ctx>) {

        let var = self.ctx.const_string(value.as_bytes(), true);
        let var_ptr = builder.build_alloca(var.get_type(), "");
        builder.build_store(var_ptr, var);

        let len = self.ctx.i32_type().const_int(value.len() as u64, false);

        (var_ptr, len)
    }

    pub fn i32(&'ctx self, value: u64) -> IntValue<'ctx> {
        self.ctx.i32_type().const_int(value, true)
    }

}




#[allow(unused)]
pub fn print_obj<'ctx>(
    module: &Module<'ctx>,
    optlv: OptimizationLevel,
) -> Result<(), Box<dyn Error>> {
    Target::initialize_native(&InitializationConfig::default())?;

    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    let target = Target::from_triple(&triple).unwrap();

    let machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            optlv,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    module.set_data_layout(&machine.get_target_data().get_data_layout());

    module.print_to_stderr();

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Assembly,
        Path::new("./output.asm"),
    )?;

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Object,
        Path::new("./output.o"),
    )?;

    Ok(())
}
