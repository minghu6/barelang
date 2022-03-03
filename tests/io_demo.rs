#![allow(unused_features)]
#![feature(box_syntax)]
#![feature(path_file_prefix)]

use std::convert::TryInto;
use std::error::Error;

use bacommon::linker::link_default;
use bacommon::runner::run_bin;
use inkwell::OptimizationLevel;
use libc;

mod common;

use proc_macros::load_vm_common_ty;

use crate::common::print_obj;
use crate::common::VMCtxHolder;



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
    let flags = holder.i32((libc::O_CREAT | libc::O_WRONLY).try_into().unwrap());
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


    // end main
    builder.build_return(Some(&i64_t.const_zero()));

    print_obj(&module, OptimizationLevel::None)?;
    println!("->: {}", module_name!());
    let output = module_name!() + ".out";
    link_default(&output)?;
    run_bin(&output)
}
