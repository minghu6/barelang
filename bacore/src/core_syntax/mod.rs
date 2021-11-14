use std::{collections::VecDeque, rc::Rc};
use std::error::Error;
use std::ops::Index;

use convert_case::{Case, Casing};
use indexmap::IndexMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicTypeEnum, FloatType, FunctionType, StructType},
    values::IntValue,
    AddressSpace,
};
use itertools::{Either, Itertools};
use lisparser::data::ListData;

use self::{
    a_fn::AFn,
    a_struct::AStruct,
    template_fn::TemplateFn,
    template_struct::TemplateStruct,
    type_anno::{ConcreteTypeAnno, TypeAnno},
};
use bacommon::error::*;
use crate::*;


pub(crate) mod a_fn;
pub(crate) mod a_struct;
pub(crate) mod a_value;
pub(crate) mod template_fn;
pub(crate) mod template_struct;
pub(crate) mod a_ns;
pub(crate) mod type_anno;
pub(crate) mod name_mangling;
pub(crate) mod hardcode_fns;
pub(crate) mod form_handel;
pub(crate) mod spec_etc;


/// Or alias as `Special Form`
pub(crate) enum CoreSyntax {
    /* Package Management */
    NS,
    InNS,

    /* Basic Struct */
    TemplateFn,
    TemplateStruct,
    Fn,
    Struct,

    Param,
    Type,
}

pub(crate) enum TopLevelSyntax {
    /* Package Management */
    NS,
    InNS,

    /* Basic Struct */
    TemplateFn,
    TemplateStruct,
    Fn,
    Struct,
}

////////////////////////////////////////////////////////////////////////////////
//// Context (namespace one2one)

pub struct CompileContext<'ctx> {
    pub(crate) template_struct_map: IndexMap<String, TemplateStruct>,
    pub(crate) template_fn_map: IndexMap<String, TemplateFn>,

    pub(crate) form_struct_map: IndexMap<String, AStruct>,
    pub(crate) form_fn_map: IndexMap<String, AFn>,

    pub(crate) struct_map: IndexMap<String, StructType<'ctx>>,
    pub(crate) fn_map: IndexMap<String, FunctionType<'ctx>>,  // vmname

    pub(crate) vmctx: &'ctx Context,
    pub(crate) vmmod: Module<'ctx>,
    pub(crate) vmbuilder: Builder<'ctx>,  // Global builder
}


impl<'ctx> CompileContext<'ctx> {
    pub(crate) fn new(name: &str, vmctx: &'ctx Context) -> Self {
        let vmmod = vmctx.create_module(name);
        let vmbuilder = vmctx.create_builder();

        Self {
            vmctx,
            vmmod,
            vmbuilder,

            template_struct_map: IndexMap::new(),
            template_fn_map: IndexMap::new(),

            form_struct_map: IndexMap::new(),
            form_fn_map: IndexMap::new(),

            struct_map: IndexMap::new(),
            fn_map: IndexMap::new(),

        }
    }

    pub(crate) fn mod_name(&self) -> String {
        self.vmmod.get_name().to_str().unwrap().to_owned()
    }
}

