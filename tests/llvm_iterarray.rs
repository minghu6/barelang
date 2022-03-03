#![allow(unused_imports)]
#![feature(path_file_prefix)]
#![feature(box_syntax)]

use bacommon::linker::link_default;
use bacommon::runner::run_bin;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::values::{
    AnyValue, BasicValue, BasicValueEnum, FunctionValue, IntValue,
    PointerValue,
};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use itertools::Itertools;

use std::convert::TryInto;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt::Display;
use std::path::Path;

mod common;

use crate::common::print_obj;
use bac::backend::codegen::{create_entry_block_alloca, isize_type};

fn codegen_inner_iter_loop<'ctx>(
    builder: &Builder<'ctx>,
    context: &'ctx Context,
    fnval: FunctionValue<'ctx>,
    elem_len: IntValue<'ctx>,
    elems_ptr: PointerValue<'ctx>,
    do_print_val: FunctionValue<'ctx>,
) -> Result<(), Box<dyn Error>> {
    /* build for */
    let int_arr_idx_alloca = create_entry_block_alloca(
        &context,
        fnval.get_first_basic_block().unwrap(),
        context.i64_type().into(),
        "iter-idx-alloca",
    );

    // idx start from 0
    let i64_t = context.i64_type();
    let idx_init_val = i64_t.const_int(0, false);
    builder.build_store(int_arr_idx_alloca, idx_init_val);
    let step_val = i64_t.const_int(1 as u64, false);

    let start_cond = builder.build_int_compare(
        IntPredicate::ULT,
        idx_init_val,
        elem_len,
        "",
    );

    let blk_loop = context.append_basic_block(fnval, "loop");
    let blk_after = context.append_basic_block(fnval, "loop:end");
    builder.build_conditional_branch(start_cond, blk_loop, blk_after);

    // for loop
    builder.position_at_end(blk_loop);
    let cur_idx = builder
        .build_load(int_arr_idx_alloca, "")
        .try_into()
        .unwrap();
    let nxt_idx = builder.build_int_add(cur_idx, step_val, "");

    builder.build_store::<IntValue>(int_arr_idx_alloca, nxt_idx.into());

    let cur_elem_ptr = unsafe {
        builder.build_in_bounds_gep(elems_ptr, &[cur_idx.into()], "")
    };

    let cur_elem = builder.build_load(cur_elem_ptr, "");
    builder.build_call(do_print_val, &[cur_elem.into()], "printi32");

    // for end test
    let end_cond =
        builder.build_int_compare(IntPredicate::ULT, nxt_idx, elem_len, "");
    builder.build_conditional_branch(end_cond, blk_loop, blk_after);
    builder.position_at_end(blk_after);

    Ok(())
}



/// Test for/array/structure
#[allow(unused)]
fn llvm_codegen() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(&module_name!());

    ///////////////////////////////////////////////////////////////////////////
    //// Code Generation

    /* declare type */
    let i64_t = context.i64_type();
    let i8_t = context.i8_type();
    let i8ptr_t = i8_t.ptr_type(AddressSpace::Generic);
    let void_type = context.void_type();
    let i32_t = context.i32_type();
    let i32ptr_t = i32_t.ptr_type(AddressSpace::Generic);

    /* structure raw_vec */
    // RawArr {
    //   cap: u64,
    //   ptr: *T
    // }
    //
    // Arr {
    //   len: usize,
    //   buf: RawArr*
    // }
    let raw_arr_t =
        context.struct_type(&[i64_t.into(), i32ptr_t.into()], false);
    let raw_arr_ptr_t = raw_arr_t.ptr_type(AddressSpace::Shared);
    let arr_t =
        context.struct_type(&[i64_t.into(), raw_arr_ptr_t.into()], false);
    module.add_global(raw_arr_t, Some(AddressSpace::Generic), "RawArr");
    module.add_global(arr_t, Some(AddressSpace::Generic), "Arr");

    /* Create Main Function */
    let fn_main_t = i64_t.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);

    /* Extern Function */

    /* prints */
    let fn_prints_t = i8ptr_t.fn_type(&[i8ptr_t.into()], false);
    let fn_prints =
        module.add_function("prints", fn_prints_t, Some(Linkage::External));
    /* printi32 */
    let fn_printi32_t = i32_t.fn_type(&[i32_t.into()], false);
    let fn_printi32_val = module.add_function(
        "printi32",
        fn_printi32_t,
        Some(Linkage::External),
    );

    /* build raw_arr.ptr */
    let int_arr = (0..10)
        .into_iter()
        .map(|x| i32_t.const_int(x as u64, false))
        .collect_vec();
    // let int_arr_val = i32_t.const_array(&int_arr[..]);

    let int_arr_capcity =
        i64_t.const_int(int_arr.len().try_into().unwrap(), false);
    let int_arr_len =
        i64_t.const_int(int_arr.len().try_into().unwrap(), false);

    let int_arr_ptr =
        builder.build_array_malloc(i32_t, int_arr_capcity, "")?;

    // builder.build_store(int_arr_ptr, int_arr_val);
    for (i, int) in int_arr.into_iter().enumerate() {
        let idx = i64_t.const_int(i as u64, false);
        let ptr = unsafe { builder.build_gep(int_arr_ptr, &[idx.into()], "") };
        builder.build_store(ptr, int);
    }

    /* build raw_arr */
    let raw_arr_val = raw_arr_t
        .const_named_struct(&[int_arr_capcity.into(), int_arr_ptr.into()]);
    let raw_arr_ptr = builder.build_malloc(raw_arr_t, "malloc RawArr")?;
    builder.build_store(raw_arr_ptr, raw_arr_val);

    /* build arr */
    let arr_val =
        arr_t.const_named_struct(&[int_arr_len.into(), raw_arr_ptr.into()]);
    let arr_ptr = builder.build_malloc(arr_t, "malloc Arr")?;
    builder.build_store(arr_ptr, arr_val);

    /* build load arr */
    let arr_len_ptr = builder
        .build_struct_gep(arr_ptr, 0, "get *(vec.len)")
        .unwrap();
    let arr_len = builder.build_load(arr_len_ptr, "");

    let arr_buf_ptr = builder
        .build_struct_gep(arr_ptr, 1, "get *(vec.buf)")
        .unwrap();
    let arr_buf = builder.build_load(arr_buf_ptr, "");

    /* build load raw_arr */
    let raw_arr_ptr_ptr = builder
        .build_struct_gep(
            arr_buf.into_pointer_value(),
            1,
            "get *(raw_vec.ptr)",
        )
        .unwrap();
    let raw_arr_ptr = builder.build_load(raw_arr_ptr_ptr, "");

    codegen_inner_iter_loop(
        &builder,
        &context,
        fn_main,
        arr_len.into_int_value(),
        raw_arr_ptr.into_pointer_value(),
        fn_printi32_val,
    )?;


    /* main return 0 */
    builder.build_return(Some(&i64_t.const_zero()));

    fn_main.verify(true);

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation

    print_obj(&module, OptimizationLevel::None)
}

#[test]
fn test_llvm_iterarray() -> Result<(), Box<dyn Error>> {
    llvm_codegen()?;

    println!("->: {}", module_name!());

    let output = module_name!() + ".out";

    link_default(&output)?;

    run_bin(&output)
}
