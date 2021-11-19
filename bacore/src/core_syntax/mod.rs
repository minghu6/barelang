use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ops::Index;
use std::{collections::VecDeque, rc::Rc};

use bacommon::etc_utils::{gen_counter, CounterType};
use bacommon::vmbuilder::builder_position_at_before;
use convert_case::{Case, Casing};
use indexmap::{IndexMap, IndexSet};
use inkwell::basic_block::BasicBlock;
use inkwell::values::{FunctionValue, InstructionValue};
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
use m6stack::Stack;

use self::a_fn::ConcreteParam;
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
pub(crate) mod hardcode_fns;
pub(crate) mod name_mangling;
pub(crate) mod spec_etc;
pub(crate) mod template_fn;
pub(crate) mod template_struct;
pub(crate) mod type_anno;



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

    root_lctx_counter: CounterType,

    pub(crate) avalue_map: HashMap<String, AValue<'ctx>>,
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
            root_lctx_counter: gen_counter(),
            avalue_map: HashMap::new(),
        }
    }

    pub(crate) fn fn_top_lctx(
        &mut self,
        params: &[ConcreteParam],
        fnval: FunctionValue<'ctx>,
        fnbody: BasicBlock<'ctx>,
    ) -> LocalContext<'ctx> {
        let mut lctx = LocalContext::empty(
            (self.root_lctx_counter)(),
            LocalContextType::FnBody(fnbody),
            None,
        );

        let binds: IndexSet<String> = params
        .iter()
        .enumerate()
        .map(|(i, param)| {
            let complete_name = lctx.complete_prefix() + &param.formal;
            let avalue = AValue {
                type_anno: param.type_anno.clone(),
                vm_val: fnval.get_nth_param(i as u32).unwrap(),
            };

            self.avalue_map.insert(complete_name, avalue);

            param.formal.clone()

        })
        .collect();

        lctx.replace_binds(binds);

        lctx
    }

    pub(crate) fn mod_name(&self) -> String {
        self.vmmod.get_name().to_str().unwrap().to_owned()
    }

    pub(crate) fn get_avalue(&self, lctx: &LocalContext, name: &str) -> Option<&AValue<'ctx>> {
        if let Some(ref complete_name) = lctx.get(name) {
            Some(self.avalue_map.get(complete_name).unwrap())
        }
        else {
            None
        }
    }


}

////////////////////////////////////////////////////////////////////////////////
//// Local Context

#[derive(Clone)]
pub(crate) struct LocalContext<'ctx>(Rc<RefCell< _LocalContext<'ctx>>>);

struct _LocalContext<'ctx> {
    pub(crate) id: usize,
    pub(crate) binds: IndexSet<String>,
    pub(crate) lctx_type: LocalContextType<'ctx>,
    pub(crate) paren: Option<LocalContext<'ctx>>,
    sub_lctx_counter: CounterType,
}

impl<'ctx> LocalContext<'ctx> {
    pub(crate) fn empty(
        id: usize,
        lctx_type: LocalContextType<'ctx>,
        paren: Option<Self>,
    ) -> Self {
        Self(Rc::new(RefCell::new(_LocalContext {
            id,
            binds: IndexSet::new(),
            lctx_type,
            paren,
            sub_lctx_counter: gen_counter(),
        })))
    }


    /// Create new `FnBody` type Local Context based itself
    pub(crate) fn get_builder(&self, vmctx: &'ctx Context) -> Builder<'ctx> {
        let mut builder = vmctx.create_builder();

        match self.0.as_ref().borrow().lctx_type {
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

    pub(crate) fn sub_empty(
        &mut self,
        binds: IndexSet<String>,
        lctx_type: LocalContextType<'ctx>,
    ) -> Self {
        Self::empty(
            (self.0.as_ref().borrow_mut().sub_lctx_counter)(),
            lctx_type,
            Some(self.clone()),
        )
    }

    pub(crate) fn id(&self) -> String {
        self.0.as_ref().borrow().id.to_string()
    }

    pub(crate) fn paren(&self) -> Option<Self> {
        self.0.as_ref().borrow().paren.clone()
    }

    pub(crate) fn complete_prefix(&self) -> String {
        let mut parens_name = vec![self.id()];

        let mut ptr = self.paren();

        while ptr.is_some() {
            let just_ptr = ptr.unwrap();

            parens_name.push(just_ptr.id());
            ptr = just_ptr.paren()
        }

        parens_name
        .into_iter()
        .rev()
        .join("-")
    }

    pub(crate) fn replace_binds(&mut self, new_binds: IndexSet<String>) {
        self.0.as_ref().borrow_mut().binds = new_binds;
    }


    pub(crate) fn get(&self, name: &str) -> Option<String> {
        if self.0.as_ref().borrow().binds.contains(name) {
            Some(self.complete_prefix() + name)
        }
        else {
            None
        }
    }

}





pub(crate) enum LocalContextType<'ctx> {
    FnBody(BasicBlock<'ctx>),   // FnBlock
    LoopBody(BasicBlock<'ctx>), // LoopBlock
    NestedBody(BasicBlock<'ctx>, InstructionValue<'ctx>), // Top Line instruction
}
