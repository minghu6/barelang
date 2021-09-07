#![allow(unused_imports)]

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::values::{AnyValue, BasicValue, BasicValueEnum};
use inkwell::AddressSpace;
use itertools::Itertools;

use std::convert::TryInto;
use std::ffi::{
    CStr, CString
};
use std::error::Error;
use std::fmt::Display;
use std::process::{Command, Stdio};
use std::{env, fs};
use std::path::Path;


////////////////////////////////////////////////////////////////////////////////
//// Running Error

#[derive(Debug)]
struct RunningError {
    code: i32
}

impl Error for RunningError {
}

impl Display for RunningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code)
    }
}

impl RunningError {
    pub fn as_box_err(code: i32) -> Box<dyn Error> {
        Box::new(Self { code })
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Running Error

#[derive(Debug)]
struct ForeignInvokeError {
    msg: String
}

impl Error for ForeignInvokeError {
}

impl Display for ForeignInvokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
#[allow(dead_code)]
impl ForeignInvokeError {
    pub fn as_box_err(msg: &str) -> Box<dyn Error> {
        Box::new(Self { msg: msg.to_owned() })
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

    /* Create Main Function */
    let fn_main_t = i64_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);

    /* Extern Function */

    /* prints */
    let s0 = "hello, barelang";

    let fn_prints_t = i8ptr_t.fn_type(&[i8ptr_t.into()], false);
    let fn_prints
    = module.add_function("prints", fn_prints_t, Some(Linkage::External));

    let s0 = "hello, barelang; 你好，裸雨";
    let mut s0_i_vec
    = s0.as_bytes().into_iter().map(|x| i8_t.const_int(*x as u64, false)).collect_vec();

    let s0_len = i64_type.const_int((s0.len() as usize).try_into().unwrap(), false);
    let s0_ptr = builder.build_array_alloca(i8ptr_t, s0_len, "xxx");
    let s0_ptr_v = i8_t.const_array(&s0_i_vec[..]);

    // codegen store
    builder.build_store(s0_ptr, s0_ptr_v);

    let ret_callsite = builder.build_call(
        fn_prints,
        &[BasicValueEnum::PointerValue(s0_ptr)],
        "prints"
    );

    let ret_basic = ret_callsite.try_as_basic_value().left().unwrap();
    let ret_arr = ret_basic.into_pointer_value();

    //let fstv = builder.build_extract_value(ret_arr, 0, "").unwrap();


    let mut fstptr = unsafe {
        let idxs = vec![i32_t.const_zero()];
        builder.build_gep(ret_arr, &idxs[..], "")
    };

    builder.build_store(fstptr, i8_t.const_int('<' as u64, false));

    builder.build_call(
        fn_prints,
        &[ret_arr.into()],
        "prints"
    );


    /* printi32 */
    let fn_printi32_t = i32_t.fn_type(&[i32_t.into()], false);
    let fn_printi32_t
    = module.add_function("printi32", fn_printi32_t, Some(Linkage::External));

    let reti32_callsite = builder.build_call(
        fn_printi32_t,
        &[i32_t.const_int(1 as u64, true).into()],
        "printi32"
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

    let new_int
    = builder.build_int_add(
        reti32_basic.into_int_value(),
         i32_t.const_int(2, true),
        ""
    );
    builder.build_call(
        fn_printi32_t,
        &[new_int.into()],
        "printi32"
    );

    builder.build_return(Some(&i64_type.const_zero()));

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation
    Target::initialize_native(&InitializationConfig::default())?;

    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    let target = Target::from_triple(&triple).unwrap();

    let machine = target.create_target_machine(
        &triple,
        "generic",
        "",
        OptimizationLevel::Default,
        RelocMode::Default,
        CodeModel::Default
    ).unwrap();

    module.set_data_layout(&machine.get_target_data().get_data_layout());

    // let dir = file!();
    // println!("{}", dir);

    machine.write_to_file(&module, inkwell::targets::FileType::Object, Path::new("./output.o"))?;

    Ok(())
}


#[test]
fn test_rsclib() -> Result<(), Box<dyn Error>> {
    rsclib_codegen()?;

    let bare_home_str = env::var("BARE_HOME").unwrap_or(".".to_string());
    let bare_home = fs::canonicalize(Path::new(
        &bare_home_str
    ))?;

    let lib_path = bare_home.join("librsc.so");

    let output = "test.out";
    let tmp_out_fn = "output.o";

    //gcc output.o libbare.so -Xlinker -rpath ./ -o main
    let gcc_link_st = Command::new("gcc")
    .arg(tmp_out_fn)
    .arg(lib_path.to_str().unwrap())
    .arg("-Xlinker")
    .arg("-rpath")
    .arg(bare_home_str)
    .arg("-o")
    .arg(output)
    .status()?;

    assert!(gcc_link_st.success(), "gcc link {}", gcc_link_st);

    //fs::remove_file(tmp_out_fn)?;

    // Run Test
    let mut test_res = Command::new(&format!("./{}", output))
    .stdin(Stdio::null())
    .stdout(Stdio::inherit())
    .spawn()?;

    let exit_st = test_res.wait()?;
    if exit_st.success() {
        Ok(())
    }
    else {
        Err(RunningError::as_box_err(exit_st.code().unwrap()))
    }

}
