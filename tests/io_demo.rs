#![allow(unused_features)]
#![feature(box_syntax)]
#![feature(path_file_prefix)]

use std::convert::TryInto;
use std::error::Error;

use bacommon::linker::link2_default;
use bacommon::runner::run_bin;
use inkwell::OptimizationLevel;
use libc;

mod common;

use proc_macros::load_vm_common_ty;

use crate::common::print_obj;
use crate::common::VMCtxHolder;

use mixin::n::PriTy;

#[test]
fn test_io() -> Result<(), Box<dyn Error>> {
    let holder = VMCtxHolder::new();

    let module = holder.init_module(&module_name!());
    let builder = holder.get_builder();
    load_vm_common_ty!(holder.ctx);

    // begin main
    let blk_main = holder.append_main(&module);
    builder.position_at_end(blk_main);

    // open it
    let fn_open = module.get_function("open").unwrap();
    let (fns, _) = holder.build_local_str(&builder, "hello.txt");
    let flags = i32_t.const_int(
        (libc::O_CREAT | libc::O_WRONLY).try_into().unwrap(),
        true
    );
    let fd = builder
        .build_call(fn_open, &[fns.into(), flags.into()], "")
        .try_as_basic_value()
        .left()
        .unwrap();

    // write to it
    let fn_write = module.get_function("write").unwrap();
    let (content, content_len) =
        holder.build_local_str(&builder, "lalal, 写入了");
    builder.build_call(
        fn_write,
        &[fd.into(), content.into(), content_len.into()],
        "",
    );

    // test nds
    let base_ty = i8_t.const_int(
        PriTy::Str.as_value() as u64, false
    );

    let (dtyids, dtyids_len) = holder.build_local_u8_array(
        &builder,
        &[
            holder.u8(PriTy::Int.as_value()),
            holder.u8(PriTy::Str.as_value())
        ]
    );

    let (dtycaps, _) = holder.build_local_u8_array(
        &builder,
        &[holder.u8(2), holder.u8(7)]
    );

    // create nds
    let fn_nds_create = module.get_function("nds_create").unwrap();
    let nds_raw = builder.build_call(
        fn_nds_create,
        &[base_ty.into(), dtyids.into(), dtycaps.into(), dtyids_len.into()],
        ""
    )
    .try_as_basic_value()
    .left()
    .unwrap();

    // assoc nds
    let (hs1, hs1_len)
        = holder.build_local_str(&builder, "hello, ");
    let (hs2, hs2_len)
         = holder.build_local_str(&builder, "world!");

    let (sk1, _)
        = holder.build_local_str(&builder, "h-,");
    let (sk2, _)
        = holder.build_local_str(&builder, "w-!");

    let (attrs, _) = holder.build_local_usize_array(
        &builder,
        &[
            size_t.const_int(0, false).into(),
            sk1.const_to_int(size_t).into()
        ]
    );


    // end main
    builder.build_return(Some(&i64_t.const_zero()));

    print_obj(&module, OptimizationLevel::None)?;
    println!("->: {}", module_name!());
    let output = module_name!() + ".out";
    link2_default(&output)
    // run_bin(&output)
}
