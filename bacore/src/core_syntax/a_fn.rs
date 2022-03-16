use std::{error::Error, marker::PhantomData, rc::Rc};

use convert_case::{Case, Casing};
use indexmap::IndexMap;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Linkage,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::InstructionValue,
};
use itertools::Itertools;
use lisparser::{data::*, pretty_printer::Dump};

use crate::core_syntax::type_anno::AddrMode;

use super::{CompileContext, LocalContext, LocalContextType, a_value::AValue, name_mangling::{concat_overload_name, rename_fn_alias, NameConcatStyle}, type_anno::ConcreteTypeAnno};
use bacommon::{error::*, vmbuilder::builder_position_at_start};

////////////////////////////////////////////////////////////////////////////////
//// Fn `Form`

#[derive(Debug, Clone)]
pub struct AFn {
    pub name: String,
    pub params: Vec<ConcreteParam>,
    pub ret: Option<ConcreteTypeAnno>,
    pub body: ListData,
}

#[derive(Debug, Clone)]
pub struct ConcreteParam {
    pub formal: String,
    pub type_anno: ConcreteTypeAnno,
}


impl AFn {
    pub fn vm_name(&self) -> String {
        let concrete_types = self
            .params
            .iter()
            .map(|param| param.type_anno.clone())
            .collect_vec();

        concat_overload_name(
            &self.name,
            &concrete_types[..],
            NameConcatStyle::Fn,
        )
    }
}

