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
}

macro_rules! id {
    () => {
        GET_GLOBAL_ID.lock().unwrap()()
    };
}


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

        impl_fn_hdr![ module |
            printf(*i8, ...) -> i32;

            open(*i8, i32, ...) -> i32;
            close(i32) -> i32;
            write(i32, *i8, usize) -> i128;

            strlen(*i8) -> usize;
        ];

        //////////////////////////////////////////////////////////////////////
        //// LibMixin Functions

        impl_fn_hdr![ module |
            nds_create(u8, *u8, *usize, usize) -> *u8;
            nds_assoc(*u8, *usize) -> *usize;
            nds_get(*u8, *usize) -> *usize;
            nds_len(*u8) -> usize;
            nds_deallocate(*u8)
        ];

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

    // pub fn build_cast_ptr_to_int(
    //     &'ctx self,
    //     builder: &Builder<'ctx>,
    //     value: PointerValue<'ctx>
    // ) -> IntValue<'ctx> {
    //     load_vm_common_ty!(self.ctx);

    //     builder.build_cast(
    //         InstructionOpcode::PtrToInt,
    //         value,
    //         size_t,
    //         ""
    //     )
    //     .into_int_value()
    // }


    //////////////////////////////////////////////////////////////////////
    //// Convenient Const

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

    pub fn bif_else(&'ctx self) {}

    // if if ... else
    pub fn bif_elif_else(&'ctx self) {}
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
