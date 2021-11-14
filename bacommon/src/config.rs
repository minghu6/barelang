use std::{
    path::PathBuf,
};

use inkwell::OptimizationLevel;



///////////////////////////////////////////////////////////////////////////
//// Compiler Config

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum OptLv {
    Debug,
    Opt(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TargetType {
    Bin,
    ReLoc,
    DyLib,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EmitType {
    LLVMIR,
    Asm,
    Obj,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VerboseLv {
    V0,
    V1,
    V2,
}

impl From<usize> for VerboseLv {
    fn from(i: usize) -> Self {
        match i {
            0 => Self::V0,
            1 => Self::V1,
            2 => Self::V2,
            _ => Self::V0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum PrintTy {
    StdErr,
    File(PathBuf),
}

impl PrintTy {
    pub fn get_path(&self) -> Option<PathBuf> {
        if let Self::File(ref path) = self {
            Some(path.clone())
        } else {
            None
        }
    }
}

pub const fn usize_len() -> usize {
    if cfg!(target_pointer_width = "64") {
        8
    } else {
        4
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompilerConfig {
    pub optlv: OptLv,
    pub target_type: TargetType,
    pub emit_type: EmitType,
    pub print_type: PrintTy,
}


impl Into<OptimizationLevel> for OptLv {
    fn into(self) -> OptimizationLevel {
        match &self {
            Self::Debug => OptimizationLevel::None,
            Self::Opt(1) => OptimizationLevel::Less,
            Self::Opt(2) => OptimizationLevel::Default,
            _ => OptimizationLevel::Aggressive,
        }
    }
}
