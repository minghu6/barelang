#![allow(unused_features)]
#![feature(box_syntax)]
#![feature(path_file_prefix)]

use std::convert::TryInto;
use std::error::Error;

use bacommon::linker::link2_default;
use bacommon::runner::run_bin;
use either::Either;
use inkwell::OptimizationLevel;
use libc;

mod common;

use mixin::n::PriTy;
use proc_macros::load_vm_common_ty;

use crate::common::print_obj;
use crate::common::VMCtxHolder;



#[test]
fn test_io() -> Result<(), Box<dyn Error>> {
    let holder = VMCtxHolder::new();

    let module = holder.init_module(&module_name!());

    holder.include_stdio(&module);
    holder.include_unistd(&module);
    holder.include_fcntl(&module);
    holder.include_mixin(&module);

    let builder = holder.get_builder();
    load_vm_common_ty!(holder.ctx);

    // begin main
    let blk_main = holder.append_main(&module);
    builder.position_at_end(blk_main);

    // open it
    let fn_open = module.get_function("open").unwrap();
    let (fns, _) = holder.build_local_str(&builder, "hello.txt");
    let flags = i32_t.const_int(
        (libc::O_CREAT | libc::O_WRONLY | libc::O_APPEND)
            .try_into()
            .unwrap(),
        true,
    );
    let mode = i32_t.const_int(0o6666, false);

    let fd = ret_as_bv!(builder.build_call(
        fn_open,
        &[fns.into(), flags.into(), mode.into()],
        ""
    ));

    // write to it
    let fn_write = module.get_function("write").unwrap();
    let (content, content_len) =
        holder.build_local_str(&builder, "_____写入了");
    let write_ok = ret_as_bv!(builder.build_call(
        fn_write,
        &[fd.into(), content.into(), content_len.into()],
        "",
    ));

    holder.build_call_printf(
        &builder,
        &module,
        "write res: %d\n",
        &[write_ok.into()],
    );

    let fn_main = module.get_function("main").unwrap();

    // TEST IF ELSE
    // compare int res > 5
    let write_ok_cast =
        builder.build_int_truncate(write_ok.into_int_value(), i32_t, "");
    let if_cond =
        holder.bsgt(&builder, write_ok_cast, holder.i32(123));
    // let (then_builder, builder) = holder.bif(&builder, if_cond, fn_main);

    let (then_builder, else_builder, builder) =
        holder.bif_else(&builder, if_cond, fn_main);

    holder.build_call_printf(
        &then_builder,
        &module,
        " Enter Then: write res: %d\n",
        &[write_ok.into()],
    );
    holder.build_call_printf(
        &else_builder,
        &module,
        " Enter Else: write res: %d\n",
        &[write_ok.into()],
    );


    // TEST LOOP
    // let counter = holder.build_alloca_counter(
    //     &builder,
    //     holder.i32(0)
    // );
    let cnt_p = holder.bcnt_init(
        &builder,
        holder.i32(0),
    );

    let (loop_builder, builder) = holder.bloop(&builder, fn_main);
    // let loop_nxt_blk = builder.get_insert_block().unwrap();
    // let loop_blk = loop_builder.get_insert_block().unwrap();

    // let loop_cont_cond = holder.bcnt_check(&loop_builder, cnt_p, holder.i32(-5));

    // let (if_builder, if_nxt_b) = holder.bif(&loop_builder, loop_cont_cond, fn_main);
    // let if_nxt_blk = if_nxt_b.get_insert_block().unwrap();

    // if_builder.build_conditional_branch(
    //     loop_cont_cond,
    //     if_nxt_blk,
    //     loop_nxt_blk
    // );

    // let loop_builder = if_nxt_b;
    let fn_sleep = module.get_function("sleep").unwrap();
    loop_builder.build_call(fn_sleep, &[holder.i32(1).into()], "");
    let cnt = holder.bload_int(&loop_builder, cnt_p);
    holder.build_call_printf(
        &loop_builder,
        &module,
        "loop %d...\n",
        &[cnt.into()],
    );
    holder.bcnt_forward(&loop_builder, cnt_p, holder.i32(-1));

    // test nds
    let base_ty = i8_t.const_int(PriTy::Str.as_value() as u64, false);

    let (dtyids, dtyids_len) = holder.build_local_const_u8_array(
        &builder,
        &[
            holder.u8(PriTy::Int.as_value()),
            holder.u8(PriTy::Str.as_value()),
        ],
    );

    let (dtycaps, _) = holder
        .build_local_const_usize_array(&builder, &[holder.usize(2), holder.usize(7)]);

    // create nds
    let fn_nds_create = module.get_function("nds_create").unwrap();
    let nds_raw = ret_as_bv!(builder.build_call(
        fn_nds_create,
        &[
            base_ty.into(),
            dtyids.into(),
            dtycaps.into(),
            dtyids_len.into()
        ],
        ""
    ));

    // assoc nds
    let fn_nds_assoc = module.get_function("nds_assoc").unwrap();

    let (hs1, hs1_len) = holder.build_local_str(&builder, "hello, ");
    let (hs2, hs2_len) = holder.build_local_str(&builder, "world!\n");

    let (sk1, _) = holder.build_local_str(&builder, "h-,");
    let (sk2, _) = holder.build_local_str(&builder, "w-!");

    let sk1 = builder.build_pointer_cast(sk1, sizeptr_t, "");

    let sk1_as_int = builder.build_ptr_to_int(sk1, size_t, "");
    let sk2_as_int = builder.build_ptr_to_int(sk2, size_t, "");

    let (attrs1, _) = holder.build_local_dyn_usize_array(
        &builder,
        &[
            holder.usize(0),
            sk1_as_int,
        ],
    );
    let (attrs2, _) = holder.build_local_dyn_usize_array(
        &builder,
        &[
            holder.usize(1),
            sk2_as_int,
        ],
    );

    let assoc_res1 = ret_as_bv!(builder.build_call(
        fn_nds_assoc,
        &[nds_raw.into(), attrs1.into()],
        ""
    ));
    let hs1_as_int = builder.build_ptr_to_int(hs1, size_t, "");
    builder.build_store(assoc_res1.into_pointer_value(), hs1_as_int);

    let assoc_res2 = ret_as_bv!(builder.build_call(
        fn_nds_assoc,
        &[nds_raw.into(), attrs2.into()],
        ""
    ));
    let hs2_as_int = builder.build_ptr_to_int(hs2, size_t, "");
    builder.build_store(assoc_res2.into_pointer_value(), hs2_as_int);

    // nds_get
    let fn_nds_get = module.get_function("nds_get").unwrap();

    let get_res_p1 = ret_as_bv!(builder.build_call(
        fn_nds_get,
        &[nds_raw.into(), attrs1.into()],
        ""
    ));
    let get_res_v1 = builder.build_load(get_res_p1.into_pointer_value(), "").into_int_value();
    let get_res_cast1 = builder.build_int_to_ptr(get_res_v1, i8ptr_t, "");

    builder.build_call(
        fn_write,
        &[fd.into(), get_res_cast1.into(), hs1_len.into()],
        "",
    );

    let get_res_p2 = ret_as_bv!(builder.build_call(
        fn_nds_get,
        &[nds_raw.into(), attrs2.into()],
        ""
    ));
    let get_res_v2 = builder.build_load(get_res_p2.into_pointer_value(), "").into_int_value();
    let get_res_cast2 = builder.build_int_to_ptr(get_res_v2, i8ptr_t, "");

    builder.build_call(
        fn_write,
        &[fd.into(), get_res_cast2.into(), hs2_len.into()],
        "",
    );

    let fn_close = module.get_function("close").unwrap();
    ret_as_bv!(builder.build_call(fn_close, &[fd.into()], ""));

    // close it


    // end main
    builder.build_return(Some(&i64_t.const_zero()));
    fn_main.verify(true);

    print_obj(&module, OptimizationLevel::None)?;
    println!("->: {}", module_name!());
    let output = module_name!() + ".out";
    link2_default(&output)?;
    run_bin(&output)
}
