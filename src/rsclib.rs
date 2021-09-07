
use inkwell::{context::Context, types::FunctionType};
use lazy_static::lazy_static;
use indexmap::{IndexMap, indexmap};

use crate::datair::{ExRefFunProto, ExRefType};

pub type ExFunTypeGetter = fn(ctx: &Context) -> FunctionType;

pub fn search_rs_lib(fname: &str) -> Option<&ExRefFunProto> {
    (*EX_RS_PROTOS).get(fname)
}

lazy_static! {
    pub static ref EX_RS_PROTOS:IndexMap<&'static str, ExRefFunProto> = indexmap! {
        "printi" => ExRefFunProto {
            name: "printi".to_owned(),
            params: vec![
                ExRefType::I64
            ],
            ret: ExRefType::I64
        },
        "prints" => ExRefFunProto {
            name: "prints".to_owned(),
            params: vec![
                ExRefType::String
            ],
            ret: ExRefType::String
        }
    };
}

