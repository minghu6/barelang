use std::error::Error;

use inkwell::{
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target,
        TargetMachine,
    },
};

use crate::config::*;
use crate::error::*;

pub struct TargetGenerator<'a, 'ctx> {
    vmmod: &'a Module<'ctx>,
    config: &'a CompilerConfig,
}

impl<'a, 'ctx> TargetGenerator<'a, 'ctx> {
    pub fn new(vmmod: &'a Module<'ctx>, config: &'a CompilerConfig) -> Self {
        Self { vmmod, config }
    }

    pub fn print(&self) -> Result<(), Box<dyn Error>> {
        match self.config.emit_type {
            EmitType::Obj => self.emit_obj(),
            EmitType::LLVMIR => self.emit_llvmir(),
            _ => todo!(),
        }
    }

    fn emit_obj(&self) -> Result<(), Box<dyn Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let triple = TargetMachine::get_default_triple();
        self.vmmod.set_triple(&triple);

        let target = Target::from_triple(&triple).unwrap();

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                self.config.optlv.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        self.vmmod
            .set_data_layout(&machine.get_target_data().get_data_layout());

        machine.write_to_file(
            &self.vmmod,
            FileType::Object,
            &self.config.print_type.get_path().unwrap(),
        )?;

        Ok(())
    }

    fn emit_llvmir(&self) -> Result<(), Box<dyn Error>> {
        if let PrintTy::File(ref path) = self.config.print_type {
            self.vmmod.print_to_file(path).map_err(|llvmstr| {
                XXXError::new_box_err(llvmstr.to_str().unwrap())
            })
        } else {
            Ok(self.vmmod.print_to_stderr())
        }
    }
}
