use std::{error::Error, marker::PhantomData};

use inkwell::{builder::Builder, values::BasicValueEnum};
use itertools::Itertools;
use lisparser::data::*;

use super::{
    LocalContext, a_value::AValue, type_anno::ConcreteTypeAnno,
    CompileContext,
};
use crate::core_syntax::{LocalContextType, name_mangling::{concat_overload_name, rename_fn_alias, NameConcatStyle}, type_anno::{self, AddrMode}};
use bacommon::{error::*, vmbuilder::builder_position_at_before};


pub(crate) struct LSPForm<'ctx> {
    ctx: &'ctx mut CompileContext<'ctx>,
    nonnillist: NonNilListData
}


impl<'ctx> LSPForm<'ctx> {

}


