use indexmap::IndexMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, StructType},
};
use itertools::Either;
use lisparser::data::ListData;
use std::error::Error;

use std::{collections::VecDeque, ops::Index};

use self::{form_struct::AStruct, form_template_struct::TemplateStruct};

pub(crate) mod form_struct;
pub(crate) mod form_template_struct;

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
//// Context (core build as form of `context`)

pub struct CompileContext<'ctx> {
    template_struct_map: IndexMap<String, TemplateStruct>,
    template_fn_map: IndexMap<TemplateFnKey, TemplateFn>,

    form_struct_map: IndexMap<String, AStruct>,

    struct_map: IndexMap<String, StructType<'ctx>>,
    fn_map: IndexMap<FnKey, FnForm>,

    vmctx: &'ctx Context,
    vmmod: Module<'ctx>,
    vmbuilder: Builder<'ctx>,
}

pub struct ThisModuleContext {}

////////////////////////////////////////////////////////////////////////////////
//// `TemplateFn` Form

pub struct TemplateFn;

////////////////////////////////////////////////////////////////////////////////
//// TemplateFnKey

pub struct TemplateFnKey {
    pub name: String,
    pub params: Vec<TemplateTypeAnno>,
}

////////////////////////////////////////////////////////////////////////////////
//// Template Type Annotation

#[derive(Debug, Clone)]
pub enum TemplateTypeAnno {
    TemplateStruct(String, Vec<usize>),
    Itself(usize),
}

////////////////////////////////////////////////////////////////////////////////
//// FnKey

pub struct FnKey {
    pub name: String,
    pub params: Vec<ConcreteTypeAnno>,
}

////////////////////////////////////////////////////////////////////////////////
//// Type Annotation

#[derive(Debug, Clone)]
pub enum ConcreteTypeAnno {
    Primitive(String),
    Struct(String),
}

impl<'ctx> ConcreteTypeAnno {
    pub(crate) fn compile(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        Ok(match &self {
            Self::Primitive(ref name) => match name.as_str() {
                "usize" | "i64" | "u64" => ctx.vmctx.i64_type(),
                "i32" | "u32" => ctx.vmctx.i32_type(),
                "i8" | "u8" => ctx.vmctx.i8_type(),
                _ => unreachable!(),
            }
            .into(),
            Self::Struct(name) => {
                if let Some(bt) = ctx.struct_map.get(name) {
                    bt.clone().into()
                } else {
                    // collect all form-struct before compile
                    let form_struct =
                        ctx.form_struct_map.get(name).unwrap().clone();

                    // circular dependency check before that
                    form_struct.compile(ctx)?
                }
            }
        })
    }
}

impl ToString for ConcreteTypeAnno {
    fn to_string(&self) -> String {
        match &self {
            ConcreteTypeAnno::Primitive(name) => name.to_owned(),
            ConcreteTypeAnno::Struct(name) => name.to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeAnno {
    Template(TemplateTypeAnno),
    Concrete(ConcreteTypeAnno),
}

////////////////////////////////////////////////////////////////////////////////
//// Param

#[derive(Debug, Clone)]
pub struct Param {
    pub formal: String,
    pub type_anno: TypeAnno,
}

#[derive(Debug, Clone)]
pub struct ConcreteParam {
    pub formal: String,
    pub type_anno: ConcreteTypeAnno,
}

////////////////////////////////////////////////////////////////////////////////
//// Fn Form

pub struct FnForm {
    pub name: String,
    pub generic: Vec<String>,
    pub params: Vec<ConcreteParam>,
    pub ret: Option<ConcreteTypeAnno>,
    pub body: Vec<ListData>,
}

////////////////////////////////////////////////////////////////////////////////
//// `NS` Form

pub struct NS {
    pub name: String,      // namespace name
    pub mods: Vec<String>, // modules name belongs to the namespace
}

impl NS {
    pub fn load_mods() {}
}

pub trait Compile<'ctx> {
    fn compile(
        &self,
        ctx: &mut CompileContext<'ctx>,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>>;
}

pub trait ConcreteTypeAnnoGetter {
    fn get_concrete_type_anno(&self) -> ConcreteTypeAnno;
}
