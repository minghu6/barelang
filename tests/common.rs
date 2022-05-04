#![allow(unused_attributes)]
#![feature(box_syntax)]
#![feature(path_file_prefix)]


use std::convert::TryInto;
use std::error::Error;
use std::marker::PhantomData;
use std::path::Path;
use std::sync::Mutex;

use bacommon::vmbuilder::builder_position_at_start;
use bacommon::etc_utils::{gen_counter, CounterType};

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::{
    BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::{IntPredicate, OptimizationLevel};
use lazy_static::lazy_static;
use either::Either;

use proc_macros::{impl_fn_hdr, load_vm_common_ty};


#[macro_export]
macro_rules! module_name {
    () => {{
        let path = std::path::Path::new(file!());

        path.file_prefix().unwrap().to_str().unwrap().to_string()
    }};
}

lazy_static! {
    pub static ref GET_GLOBAL_ID: Mutex<CounterType> = Mutex::new(gen_counter());
    // pub static ref VM_CTX_HOLDER: Mutex<VMCtxHolder<'static>> = Mutex::new(VMCtxHolder::new());
}

macro_rules! id {
    () => {
        GET_GLOBAL_ID.lock().unwrap()()
    };
}

// macro_rules! holder {
//     () => {
//         VM_CTX_HOLDER.lock().unwrap()
//     };
// }


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

    //////////////////////////////////////////////////////////////////////
    //// Include Header (POSIX + CORE)
    //////////////////////////////////////////////////////////////////////

    ///////////////////////////////////
    //// POSIX

    pub fn include_fcntl(&'ctx self, module: &Module<'ctx>) {
        impl_fn_hdr![ module |
            open(*i8, i32, ...) -> i32;
        ];
    }

    pub fn include_stdio(&'ctx self, module: &Module<'ctx>) {
        impl_fn_hdr![ module |
            printf(*i8, ...) -> i32;
        ];
    }

    pub fn include_string(&'ctx self, module: &Module<'ctx>) {
        impl_fn_hdr![ module |
            strlen(*i8) -> usize;
        ];
    }

    pub fn include_unistd(&'ctx self, module: &Module<'ctx>) {
        impl_fn_hdr![ module |
            write(i32, *i8, usize) -> i128;
            close(i32) -> i32;
            sleep(u32) -> u32;
        ];
    }


    ///////////////////////////////////
    //// Core

    pub fn include_mixin(&'ctx self, module: &Module<'ctx>) {
        impl_fn_hdr![ module |
            nds_create(u8, *u8, *usize, usize) -> *u8;
            nds_assoc(*u8, *usize) -> *usize;
            nds_get(*u8, *usize) -> *usize;
            nds_len(*u8) -> usize;
            nds_deallocate(*u8)
        ];
    }


    pub fn init_module(&'ctx self, modname: &str) -> Module<'ctx> {
        let module = self.ctx.create_module(modname);

        // self.include_stdio(&module);

        module
    }

    pub fn get_builder(&'ctx self) -> Builder<'ctx> {
        self.ctx.create_builder()
    }

    pub fn get_builder_at_end(
        &'ctx self,
        blk: BasicBlock<'ctx>,
    ) -> Builder<'ctx> {
        let builder = self.get_builder();

        builder.position_at_end(blk);

        builder
    }

    pub fn get_builder_at_start(
        &'ctx self,
        blk: BasicBlock<'ctx>,
    ) -> Builder<'ctx> {
        let builder = self.get_builder();

        builder_position_at_start(&builder, blk);

        builder
    }

    pub fn append_main(&'ctx self, module: &Module<'ctx>) -> BasicBlock<'ctx> {
        load_vm_common_ty!(self.ctx);

        let fn_main_t = i64_t.fn_type(&[], false);
        let fn_main = module.add_function("main", fn_main_t, None);

        self.ctx.append_basic_block(fn_main, "blk_main")
    }


    //////////////////////////////////////////////////////////////////////
    //// Convenient Build
    //////////////////////////////////////////////////////////////////////

    pub fn bload_int(
        &'ctx self,
        builder: &Builder<'ctx>,
        var: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        builder.build_load(var, "").into_int_value()
    }

    pub fn bcnt_init(
        &'ctx self,
        builder: &Builder<'ctx>,
        init: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        load_vm_common_ty!(self.ctx);

        let var = builder.build_alloca(i32_t, "");
        builder.build_store(var, init);

        var
    }

    pub fn bcnt_forward(
        &'ctx self,
        builder: &Builder<'ctx>,
        var: PointerValue<'ctx>,
        step: IntValue<'ctx>
    )
    {
        let val = self.bload_int(builder, var);
        let nxt = builder.build_int_add(val, step, "");
        builder.build_store(var, nxt);
    }

    /// (low, high)
    pub fn bcnt_check(
        &'ctx self,
        builder: &Builder<'ctx>,
        var: PointerValue<'ctx>,
        test: Either<IntValue<'ctx>, IntValue<'ctx>>
    ) -> IntValue<'ctx>
    {
        let val = self.bload_int(builder, var);

        match test {
            Either::Left(low) => {
                self.bsgt(builder, val, low)
            },
            Either::Right(high) => {
                self.bsgt(builder, high, val)
            },
        }
    }

    pub fn build_local_str(
        &'ctx self,
        builder: &Builder<'ctx>,
        value: &str,
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        load_vm_common_ty!(self.ctx);
        let var = self.ctx.const_string(value.as_bytes(), true);
        let len = self.usize(value.len());

        let var_ptr = builder.build_alloca(var.get_type(), "");
        builder.build_store(var_ptr, var);

        let var_ptr_cast = builder
            .build_bitcast(var_ptr, i8ptr_t, "")
            .into_pointer_value();

        (var_ptr_cast, len)
    }

    /// (*u8, len)
    pub fn build_local_const_u8_array(
        &'ctx self,
        builder: &Builder<'ctx>,
        values: &[IntValue<'ctx>],
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        load_vm_common_ty!(self.ctx);

        let var= i8_t.const_array(values);
        let len = self.usize((values.len() as u64).try_into().unwrap());

        let var_ptr = builder.build_alloca(
            var.get_type(),
            ""
        );
        builder.build_store(var_ptr, var);

        let var_ptr_cast = builder
            .build_bitcast(var_ptr, i8ptr_t, "")
            .into_pointer_value();

        (var_ptr_cast, len)
    }

    /// (*usize, len)
    pub fn build_local_const_usize_array(
        &'ctx self,
        builder: &Builder<'ctx>,
        values: &[IntValue<'ctx>],
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        load_vm_common_ty!(self.ctx);

        let var= size_t.const_array(values);
        let len = self.usize((values.len() as u64).try_into().unwrap());

        let var_ptr = builder.build_alloca(
            var.get_type(),
            ""
        );
        builder.build_store(var_ptr, var);

        let var_ptr_cast = builder
            .build_bitcast(var_ptr, sizeptr_t, "")
            .into_pointer_value();

        (var_ptr_cast, len)
    }

    /// (*usize, len)
    pub fn build_local_dyn_usize_array(
        &'ctx self,
        builder: &Builder<'ctx>,
        values: &[IntValue<'ctx>],
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        load_vm_common_ty!(self.ctx);

        let len = self.usize((values.len() as u64).try_into().unwrap());

        let var_ptr = builder.build_array_alloca(
            size_t,
            len,
            ""
        );
        for (i, value) in values.into_iter().enumerate() {
            let idx = self.usize(i);
            let ptr = unsafe {
                builder.build_in_bounds_gep(var_ptr, &[idx], "")
            };
            builder.build_store::<IntValue>(ptr, (*value).into());
        }

        let var_ptr_cast = builder
            .build_bitcast(var_ptr, sizeptr_t, "")
            .into_pointer_value();

        (var_ptr_cast, len)
    }

    pub fn build_call_printf(
        &'ctx self,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        fcs: &str,
        values: &[BasicMetadataValueEnum<'ctx>],
    ) {
        let (fcs_p, _) = self.build_local_str(builder, fcs);
        let fn_printf = module.get_function("printf").unwrap();

        let mut args = vec![fcs_p.into()];
        args.extend_from_slice(values);

        builder.build_call(fn_printf, &args[..], "");
    }


    //////////////////////////////////////////////////////////////////////
    //// Convenient Const
    //////////////////////////////////////////////////////////////////////

    // i8_t
    pub fn u8(&'ctx self, value: u8) -> IntValue<'ctx> {
        load_vm_common_ty!(self.ctx);

        i8_t.const_int(value as u64, false)
    }

    pub fn i32(&'ctx self, value: i32) -> IntValue<'ctx> {
        load_vm_common_ty!(self.ctx);

        i32_t.const_int(value as u64, false)
    }

    // size_t
    pub fn usize(&'ctx self, value: usize) -> IntValue<'ctx> {
        load_vm_common_ty!(self.ctx);

        size_t.const_int(value as u64, false)
    }


    //////////////////////////////////////////////////////////////////////
    //// Convenient Cmp
    //////////////////////////////////////////////////////////////////////

    pub fn bsgt(
        &'ctx self,
        builder: &Builder<'ctx>,
        x: IntValue<'ctx>,
        y: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        builder.build_int_compare(IntPredicate::SGT, x, y, "")
    }

    pub fn bsge(
        &'ctx self,
        builder: &Builder<'ctx>,
        x: IntValue<'ctx>,
        y: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        builder.build_int_compare(IntPredicate::SGE, x, y, "")
    }


    //////////////////////////////////////////////////////////////////////
    //// Control Flow
    //////////////////////////////////////////////////////////////////////

    pub fn bif(
        &'ctx self,
        builder: &Builder<'ctx>,
        cond: IntValue<'ctx>,
        fnv: FunctionValue<'ctx>,
    ) -> (Builder<'ctx>, Builder<'ctx>) // (then-builder, finally-builder)
    {
        let uid = id!();
        let then_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_then_{}", uid)
        );
        let finally_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_finally_{}", uid)
        );

        builder.build_conditional_branch(cond, then_blk, finally_blk);

        let then_builder = self.get_builder_at_end(then_blk);
        let finally_builder = self.get_builder_at_start(finally_blk);

        then_builder.build_unconditional_branch(finally_blk);
        builder_position_at_start(&then_builder, then_blk);

        (then_builder, finally_builder)
    }

    /// (then-builder, else-builder, finally-builder)
    pub fn bif_else(
        &'ctx self,
        builder: &Builder<'ctx>,
        cond: IntValue<'ctx>,
        fnv: FunctionValue<'ctx>,
    ) -> (Builder<'ctx>, Builder<'ctx>, Builder<'ctx>)
    {
        let uid = id!();
        let then_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_then_{}", uid)
        );
        let else_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_else_{}", uid)
        );
        let finally_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_finally_{}", uid)
        );

        builder.build_conditional_branch(cond, then_blk, else_blk);

        let then_builder = self.get_builder_at_end(then_blk);
        let else_builder = self.get_builder_at_end(else_blk);

        let finally_builder = self.get_builder_at_start(finally_blk);

        then_builder.build_unconditional_branch(finally_blk);
        builder_position_at_start(&then_builder, then_blk);

        else_builder.build_unconditional_branch(finally_blk);
        builder_position_at_start(&else_builder, else_blk);

        (then_builder, else_builder, finally_builder)
    }

    /// if elif, elif, ... else
    pub fn bif_elif_else(
        &'ctx self,
        conds: &[IntValue<'ctx>],
        fnv: FunctionValue<'ctx>,
    ) -> (Vec<Builder<'ctx>>, Builder<'ctx>)
    {
        todo!()
    }


    /// loop (without if break)
    /// -> Loop Builder
    pub fn bloop(
        &'ctx self,
        builder: &Builder<'ctx>,
        fnv: FunctionValue<'ctx>,
    ) -> (Builder<'ctx>, Builder<'ctx>)
    {
        let uid = id!();
        let loop_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_loop_{}", uid)
        );
        let finally_blk = self.ctx.append_basic_block(
            fnv,
            &format!("blk_finally_{}", uid)
        );

        let loop_builder = self.get_builder_at_end(loop_blk);
        let finally_builder = self.get_builder_at_start(finally_blk);

        builder.build_unconditional_branch(loop_blk);
        loop_builder.build_unconditional_branch(loop_blk);
        builder_position_at_start(&loop_builder, loop_blk);

        (loop_builder, finally_builder)
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


#[macro_export]
macro_rules! ret_as_bv {
    ($ret: expr) => {{
        let ret = $ret;
        ret.try_as_basic_value().left().unwrap()
    }};
}
