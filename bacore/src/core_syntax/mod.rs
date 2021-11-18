use std::collections::HashSet;
use std::error::Error;
use std::ops::Index;
use std::{collections::VecDeque, rc::Rc};

use bacommon::etc::{CounterType, gen_counter};
use bacommon::vmbuilder::builder_position_at_before;
use convert_case::{Case, Casing};
use indexmap::IndexMap;
use inkwell::basic_block::BasicBlock;
use inkwell::values::InstructionValue;
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

use self::a_value::AValue;
use self::{
    a_fn::AFn,
    a_struct::AStruct,
    template_fn::TemplateFn,
    template_struct::TemplateStruct,
    type_anno::{ConcreteTypeAnno, TypeAnno},
};
use crate::*;
use bacommon::error::*;

pub(crate) mod a_fn;
pub(crate) mod a_ns;
pub(crate) mod a_struct;
pub(crate) mod a_value;
pub(crate) mod form_compilation;
pub(crate) mod hardcode_fns;
pub(crate) mod name_mangling;
pub(crate) mod spec_etc;
pub(crate) mod template_fn;
pub(crate) mod template_struct;
pub(crate) mod type_anno;

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
    pub(crate) fn_map: IndexMap<String, FunctionType<'ctx>>, // vmname

    pub(crate) vmctx: &'ctx Context,
    pub(crate) vmmod: Module<'ctx>,

    _lctx_vec: Vec<LocalContext<'ctx>>,

}

impl<'ctx> CompileContext<'ctx> {
    pub(crate) fn new(name: &str, vmctx: &'ctx Context) -> Self {
        let vmmod = vmctx.create_module(name);

        Self {
            vmctx,
            vmmod,

            template_struct_map: IndexMap::new(),
            template_fn_map: IndexMap::new(),

            form_struct_map: IndexMap::new(),
            form_fn_map: IndexMap::new(),

            struct_map: IndexMap::new(),
            fn_map: IndexMap::new(),
            _lctx_vec: Vec::new(),
        }
    }

    pub(crate) fn create_lctx(
        &mut self,
        binds: IndexMap<String, AValue<'ctx>>,
        lctx_type: LocalContextType<'ctx>,
        paren: Option<&LocalContext<'ctx>>,
    ) -> &LocalContext<'ctx> {
        self._lctx_vec.push(LocalContext::new(binds, lctx_type, paren.cloned()));

        if let Some(lctx) = self._lctx_vec.last() {
            lctx
        }
        else {
            unreachable!()
        }
    }

    pub(crate) fn get_lctx(&self) -> &LocalContext<'ctx> {
        self._lctx_vec.last().unwrap()
    }

    pub(crate) fn mod_name(&self) -> String {
        self.vmmod.get_name().to_str().unwrap().to_owned()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Local Context

#[derive(Clone)]
pub(crate) struct LocalContext<'ctx>(Rc<_LocalContext<'ctx>>);

struct _LocalContext<'ctx> {
    pub(crate) binds: IndexMap<String, AValue<'ctx>>,
    pub(crate) lctx_type: LocalContextType<'ctx>,
    pub(crate) paren: Option<LocalContext<'ctx>>,
}

impl<'ctx> LocalContext<'ctx> {
    pub(crate) fn new(
        binds: IndexMap<String, AValue<'ctx>>,
        lctx_type: LocalContextType<'ctx>,
        paren: Option<Self>,
    ) -> Self {
        Self(Rc::new(_LocalContext {
            binds,
            lctx_type,
            paren,
        }))
    }

    pub(crate) fn lctx_type(&self) -> &LocalContextType<'ctx> {
        &self.0.lctx_type
    }

    /// Create new `FnBody` type Local Context based itself
    pub(crate) fn get_builder(&self, vmctx: &'ctx Context) -> Builder<'ctx> {
        let mut builder = vmctx.create_builder();

        match self.0.lctx_type {
            LocalContextType::FnBody(fnbody) => {
                builder_position_at_before(&mut builder, fnbody)
            }
            LocalContextType::LoopBody(loopbody) => {
                builder.position_at_end(loopbody)
            }
            LocalContextType::NestedBody(nestedbody, top_inst) => {
                builder.position_at(nestedbody, &top_inst)
            }
        }

        builder
    }

    pub(crate) fn get(&self, name: &str) -> Option<&AValue> {
        self.0.binds.get(name)
    }
}

pub(crate) enum LocalContextType<'ctx> {
    FnBody(BasicBlock<'ctx>),   // FnBlock
    LoopBody(BasicBlock<'ctx>), // LoopBlock
    NestedBody(BasicBlock<'ctx>, InstructionValue<'ctx>), // Top Line instruction
}
