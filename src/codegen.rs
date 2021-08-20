#![allow(unused_imports)]

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicTypeEnum, FunctionType, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};

use lazy_static::lazy_static;

use indexmap::indexset;

use std::error::Error;
use std::path::Path;
use std::env;

use crate::baredata::*;
use crate::*;
use crate::rslib::search_rs_lib;

#[allow(unused)]
fn hardcode_codegen() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let builder = context.create_builder();

    ///////////////////////////////////////////////////////////////////////////
    //// Code Generation

    /* declare type */
    let i64_type = context.i64_type();
    // let void_type = context.void_type();

    let fn_sum_t = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    let fn_sum = module.add_function("sum", fn_sum_t, None);
    let blk_sum = context.append_basic_block(fn_sum, "sumblk");

    builder.position_at_end(blk_sum);

    let x = fn_sum.get_nth_param(0).unwrap().into_int_value();
    let y = fn_sum.get_nth_param(1).unwrap().into_int_value();
    let z = fn_sum.get_nth_param(2).unwrap().into_int_value();

    let sum = builder.build_int_add(x, y, "sum");
    let sum = builder.build_int_add(sum, z, "sum");

    /* Extern Function */
    let fn_putchard_t = i64_type.fn_type(&[i64_type.into()], false);
    let fn_putchard
    = module.add_function("printi", fn_putchard_t, Some(Linkage::External));

    builder.build_call(
        fn_putchard,
        &[BasicValueEnum::IntValue(sum.clone())],
        "printi"
    );

    builder.build_return(Some(&sum));

    /* Create Main Function */
    let fn_main_t = i64_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);
    let const_1 = BasicValueEnum::IntValue(i64_type.const_int(1, false));
    let const_2 = BasicValueEnum::IntValue(i64_type.const_int(2, false));
    let const_3 = BasicValueEnum::IntValue(i64_type.const_int(3, false));

    builder.build_call(
        fn_sum,
        &[const_1, const_2, const_3],
        "sum"
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

pub fn codegen(frame_vec: Vec<StackFrame>) -> Result<(), Box<dyn Error>> {

    let context = Context::create();
    let module = context.create_module("test");
    let builder = context.create_builder();

    /* Create Main Function */
    let i64_type = context.i64_type();
    let fn_main_t = i64_type.fn_type(&[], false);
    let fn_main = module.add_function("main", fn_main_t, None);
    let blk_main = context.append_basic_block(fn_main, "mainblk");
    builder.position_at_end(blk_main);

    /* rs external function */

    for frame in frame_vec.iter() {
        let frame_ref = frame.frame_ref();

        for blkstmtref in frame_ref.blockstmts.iter() {
            let block_stmt = blkstmtref.block_stmt_ref();
            match &*block_stmt {
                BaBlockStmt::Stmt(stmt) => {
                    match stmt {
                        BaStmt::Expr(expr) => {
                            match expr {
                                BaExpr::FunCall(funcall) => {
                                    // search fun ref
                                    let fid = &funcall.name;

                                    let fnval;
                                    match fid.splid {
                                        Some(BaSplId::RS) => {
                                            let funtype;
                                            if let Some(getter) = search_rs_lib(&fid.name) {
                                                funtype = getter(&context);
                                            }
                                            else {
                                                unreachable!()
                                            }

                                            if let Some(_fnval) = module.get_function(&fid.name) {
                                                fnval = _fnval;
                                            }
                                            else {
                                                fnval = module.add_function(&fid.name, funtype, Some(Linkage::External));
                                            }
                                        },
                                        None => {
                                            unreachable!()
                                        }
                                    }

                                    // codegen fun call args
                                    let args_vec = codegen_args(&context, funcall);

                                    println!("args_vec: {:?}", args_vec);
                                    builder.build_call(
                                        fnval,
                                        &args_vec[..],
                                        &fid.name
                                    );

                                },
                                _ => {},
                            }
                        }
                        BaStmt::Empty => {}
                    }
                },
                _ => {}
            }
        }
    }

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

    let pwd = env::current_dir().unwrap();
    let output_path = pwd.join("output.o");

    machine.write_to_file(&module, inkwell::targets::FileType::Object, Path::new(&output_path))?;

    Ok(())
}


// fn codegen_moudle<'a>(frame_vec: Vec<StackFrame>, ) -> (Module<'a>, Context) {


//     (module, context)
// }

/// codegen after flatten the expression
fn codegen_args<'a>(context: &'a Context, funcall: &BaFunCall) -> Vec<BasicValueEnum<'a>> {
    let mut args_vec = Vec::<BasicValueEnum>::new();

    funcall.args.iter().for_each(|arg| {
        match arg {
            BaExpr::Pri(pri) => {
                match pri {
                    BaPri::Id(idcell) => {
                        let idref = idcell.as_ref().borrow();
                        let val = &idref.value;

                        match val {
                            BaVal::Num(num) => {
                                match num {
                                    BaNum::I64(v) => {
                                        let i64_t = context.i64_type();
                                        args_vec.push(
                                            BasicValueEnum::IntValue(i64_t.const_int(*v as u64, true))
                                        );
                                    },
                                    BaNum::USize(v) => {
                                        let i64_t = context.i64_type();
                                        args_vec.push(
                                            BasicValueEnum::IntValue(i64_t.const_int(*v as u64, true))
                                        );
                                    },
                                    _ => {}
                                }
                            },
                            _ => {}
                        }
                    },
                    BaPri::Val(val) => {
                        match val {
                            BaVal::Num(num) => {
                                match num {
                                    BaNum::I64(v) => {
                                        let i64_t = context.i64_type();
                                        args_vec.push(
                                            BasicValueEnum::IntValue(i64_t.const_int(*v as u64, true))
                                        );
                                    },
                                    BaNum::USize(v) => {
                                        let i64_t = context.i64_type();
                                        args_vec.push(
                                            BasicValueEnum::IntValue(i64_t.const_int(*v as u64, true))
                                        );
                                    },
                                    _ => {}
                                }
                            },
                            _ => {}
                        }
                    },
                    //_ => {}
                }
            },
            _ => {}
        }
    });

    args_vec
}

