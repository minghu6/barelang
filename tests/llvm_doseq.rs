#![allow(unused_imports)]
#![feature(path_file_prefix)]

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
use bac::{link_default, run_bin};

/// ```none
/// struct SeqGenCtx {
///     res: isize
///     state: isize (same with value, = res + 1)
/// }
/// ```
fn codegen_generator<'ctx>(
    builder: &'ctx Builder,
    module: Module<'ctx>,
    context: &'ctx Context,
    start_val: IntValue<'ctx>,
    step_val: IntValue<'ctx>,
) -> (PointerValue<'ctx>, FunctionValue<'ctx>, Module<'ctx>) {
    let isize_t = isize_type(&context);
    let void_t = context.void_type();

    /* Init SeqGenCtx */
    let seq_gen_ctx_t =
        context.struct_type(&[isize_t.into(), isize_t.into()], false);

    let seq_gen_ctx_ptr = builder.build_alloca(seq_gen_ctx_t, "SeqGenCtx");

    builder.build_store(
        seq_gen_ctx_ptr,
        seq_gen_ctx_t
            .const_named_struct(&[start_val.into(), start_val.into()]),
    );

    /* Build Generator Yield FunHdr */
    let seq_gen_yield_fn_t = void_t
        .fn_type(&[seq_gen_ctx_t.ptr_type(AddressSpace::Local).into()], false);

    let seq_gen_yield_fn_val =
        module.add_function("seq_yield", seq_gen_yield_fn_t, None);

    /* Build Generatr 1st Block */
    let seq_gen_yield_blk =
        context.append_basic_block(seq_gen_yield_fn_val, "SeqGenYieldFun");

    let fn_builder = context.create_builder();
    fn_builder.position_at_end(seq_gen_yield_blk);

    /* Update Ctx */
    let param_ctx_ptr = seq_gen_yield_fn_val
        .get_nth_param(0)
        .unwrap()
        .into_pointer_value();

    let res_ptr = fn_builder.build_struct_gep(param_ctx_ptr, 0, "").unwrap();
    let state_ptr = fn_builder.build_struct_gep(param_ctx_ptr, 1, "").unwrap();
    let state_val = fn_builder.build_load(state_ptr, "").into_int_value();

    // let's do a little trick in updating...
    fn_builder.build_store(res_ptr, state_val);

    let new_state_val = fn_builder.build_int_add(state_val, step_val, "");

    fn_builder.build_store(state_ptr, new_state_val);
    fn_builder.build_return(None);

    (seq_gen_ctx_ptr, seq_gen_yield_fn_val, module)
}

fn codegen_iter_seq<'ctx>(
    builder: &'ctx Builder<'ctx>,
    module: Module<'ctx>,
    context: &'ctx Context,
    fnval: FunctionValue<'ctx>,
) -> Module<'ctx> {
    let isize_t = isize_type(&context);

    // -2:1:..
    let (ctx_ptr, gen_fn_val, module) = codegen_generator(
        &builder,
        module,
        &context,
        isize_t.const_int((0 - 2) as u64, true),
        isize_t.const_int(1, true),
    );

    /* Link Loop Block */
    let loopblk = context.append_basic_block(fnval, "");
    builder.build_unconditional_branch(loopblk);

    builder.position_at_end(loopblk);
    // yield
    builder.build_call(gen_fn_val, &[ctx_ptr.into()], "");

    // get yield value
    let res_ptr = builder.build_struct_gep(ctx_ptr, 0, "").unwrap();
    let res_val = builder.build_load(res_ptr, "");

    /* Print Yield Value */
    let printi32 = module.get_function("printi32").unwrap();
    builder.build_call(printi32, &[res_val], "");

    let sleep = module.get_function("sleep").unwrap();
    builder.build_call(sleep, &[isize_t.const_int(1, false).into()], "");

    builder.build_unconditional_branch(loopblk);

    module
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

    /* Create Main Function */
    let fn_main_t = i64_t.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);

    /* Declare Extern Function */

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
    /* sleep */
    let fn_sleep_t = i64_t.fn_type(&[i64_t.into()], false);
    let fn_sleep =
        module.add_function("sleep", fn_sleep_t, Some(Linkage::External));

    let module = codegen_iter_seq(&builder, module, &context, fn_main);

    let blk_seqend = context.append_basic_block(fn_main, "doseq:end");
    builder.position_at_end(blk_seqend);

    /* main return 0 */
    builder.build_return(Some(&i64_t.const_zero()));

    fn_main.verify(true);

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation

    print_obj(&module, OptimizationLevel::None)
}

#[test]
fn test_llvm_doseq() -> Result<(), Box<dyn Error>> {
    llvm_codegen()?;

    println!("->: {}", module_name!());

    let output = module_name!() + ".out";

    link_default(&output)?;

    run_bin(&output)
}
