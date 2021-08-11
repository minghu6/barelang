use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::FunctionType;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};

use std::error::Error;
use std::path::Path;


fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let builder = context.create_builder();

    ///////////////////////////////////////////////////////////////////////////
    //// Code Generation

    let i64_type = context.i64_type();
    let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    let fn_sum = module.add_function("sum", fn_type, None);
    let basic_block = context.append_basic_block(fn_sum, "entry");

    builder.position_at_end(basic_block);

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
        &[BasicValueEnum::IntValue(i64_type.const_int(55, false))],
        "printi"
    );

    builder.build_return(Some(&sum));

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
