
use inkwell::{context::Context, types::FunctionType};
use lazy_static::lazy_static;
use indexmap::{IndexMap, indexmap};

pub type ExFunTypeGetter = fn(ctx: &Context) -> FunctionType;

pub fn search_rs_lib(fname: &str) -> Option<&ExFunTypeGetter> {
    (*EX_RS_PROTOS).get(fname)
}

lazy_static! {
    pub static ref EX_RS_PROTOS:IndexMap<&'static str, ExFunTypeGetter> = indexmap! {
        "printi" => printi as ExFunTypeGetter
    };
}


fn printi(ctx: &Context) -> FunctionType {
    let i64_t = ctx.i64_type();
    i64_t.fn_type(&[i64_t.into()], false)
}
