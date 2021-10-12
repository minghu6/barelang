use inkwell::module::{Module};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;
use std::error::Error;
use std::path::Path;

#[macro_export]
macro_rules! module_name {
    () => {{
        let path = std::path::Path::new(file!());

        path.file_prefix().unwrap().to_str().unwrap().to_string()
    }};
}

#[allow(unused)]
pub fn print_obj<'ctx>(module: &Module<'ctx>, optlv: OptimizationLevel) -> Result<(), Box<dyn Error>> {
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
