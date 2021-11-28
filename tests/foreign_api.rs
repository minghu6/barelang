#![allow(unused_imports)]
#![feature(path_file_prefix)]

use bacommon::linker::link_default;
use bacommon::runner::run_bin;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::values::{AnyValue, BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use itertools::Itertools;

use std::convert::TryInto;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt::Display;
use std::path::Path;

mod common;



////////////////////////////////////////////////////////////////////////////////
//// Running Error

#[derive(Debug)]
struct ForeignInvokeError {
    msg: String,
}

impl Error for ForeignInvokeError {}

impl Display for ForeignInvokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
#[allow(dead_code)]
impl ForeignInvokeError {
    pub fn as_box_err(msg: &str) -> Box<dyn Error> {
        Box::new(Self {
            msg: msg.to_owned(),
        })
    }
}

#[allow(unused)]
fn rsclib_codegen() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let builder = context.create_builder();

    ///////////////////////////////////////////////////////////////////////////
    //// Code Generation

    /* declare type */
    let i64_type = context.i64_type();
    let i8_t = context.i8_type();
    let i8ptr_t = i8_t.ptr_type(AddressSpace::Generic);
    let void_type = context.void_type();
    let i32_t = context.i32_type();
    let i32ptr_t = i32_t.ptr_type(AddressSpace::Generic);
    let f64_t = context.f64_type();

    /* Create Main Function */
    let fn_main_t = i64_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);

    /* Extern Function */
    /* C printf */
    let fn_cprintf_t = i32ptr_t.fn_type(&[i8ptr_t.into()], true);
    let fn_cprintf = module.add_function("printf", fn_cprintf_t, Some(Linkage::External));

    let ctx_s = context.const_string("printf: p-int: %d, p-float: %f, p-s: %s\n".as_bytes(), true);
    let ctrl_s_ptr = builder.build_alloca(ctx_s.get_type(), "");
    builder.build_store(ctrl_s_ptr, ctx_s);

    let s1 = context.const_string("this is tail string".as_bytes(), true);
    let s1_ptr = builder.build_alloca(s1.get_type(), "");
    builder.build_store(s1_ptr, s1);

    builder.build_call(
        fn_cprintf,
        &[
                ctrl_s_ptr.into(),
                i32_t.const_int(12345, true).into(),
                f64_t.const_float(123.45).into(),
                s1_ptr.into()
            ],
        ""
    );


    /* prints */
    let s0 = "hello, barelang";

    let fn_prints_t = i8ptr_t.fn_type(&[i8ptr_t.into()], false);
    let fn_prints = module.add_function("prints", fn_prints_t, Some(Linkage::External));

    let s0 = "hello, barelang; 你好，裸雨";
    let mut s0_i_vec = s0
        .as_bytes()
        .into_iter()
        .map(|x| i8_t.const_int(*x as u64, false))
        .collect_vec();

    let s0_len = i64_type.const_int((s0.len() as usize).try_into().unwrap(), false);
    let s0_ptr = builder.build_array_alloca(i8ptr_t, s0_len, "xxx");
    let s0_ptr_v = i8_t.const_array(&s0_i_vec[..]);

    // codegen store
    builder.build_store(s0_ptr, s0_ptr_v);

    let ret_callsite =
        builder.build_call(fn_prints, &[BasicValueEnum::PointerValue(s0_ptr).into()], "prints");

    let ret_basic = ret_callsite.try_as_basic_value().left().unwrap();
    let ret_arr = ret_basic.into_pointer_value();

    //let fstv = builder.build_extract_value(ret_arr, 0, "").unwrap();

    let mut fstptr = unsafe {
        let idxs = vec![i32_t.const_zero()];
        builder.build_gep(ret_arr, &idxs[..], "")
    };

    builder.build_store(fstptr, i8_t.const_int('<' as u64, false));

    builder.build_call(fn_prints, &[ret_arr.into()], "prints");

    /* printi32 */
    let fn_printi32_t = i32_t.fn_type(&[i32_t.into()], false);
    let fn_printi32_t = module.add_function("printi32", fn_printi32_t, Some(Linkage::External));

    let reti32_callsite = builder.build_call(
        fn_printi32_t,
        &[i32_t.const_int(1 as u64, true).into()],
        "printi32",
    );

    let reti32_basic = reti32_callsite.try_as_basic_value().left().unwrap();

    // let reti32p = match reti32_basic {
    //     BasicValueEnum::PointerValue(ptr) => {
    //         ptr
    //     },
    //     _ => unreachable!()
    // };

    // let reti32v = builder.build_load(reti32p, "");

    // let real_reti32v = match reti32v {
    //     BasicValueEnum::IntValue(int) => {
    //         int.const_cast(i32_t, true)
    //     },
    //     _ => unreachable!()
    // };

    let new_int =
        builder.build_int_add(reti32_basic.into_int_value(), i32_t.const_int(2, true), "");
    builder.build_call(fn_printi32_t, &[new_int.into()], "printi32");

    builder.build_return(Some(&i64_type.const_zero()));

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation
    Target::initialize_native(&InitializationConfig::default())?;

    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    let target = Target::from_triple(&triple).unwrap();

    let machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    module.set_data_layout(&machine.get_target_data().get_data_layout());

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Object,
        Path::new("./output.o"),
    )?;

    Ok(())
}

#[test]
fn test_rsclib() -> Result<(), Box<dyn Error>> {
    rsclib_codegen()?;

    println!("->: {}", module_name!());

    let output = module_name!() + ".out";

    link_default(&output)?;

    run_bin(&output)
    // Ok(())
}
