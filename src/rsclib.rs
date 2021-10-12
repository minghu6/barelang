
use inkwell::{context::Context, types::FunctionType};
use lazy_static::lazy_static;
use indexmap::{IndexMap, indexmap};

use crate::middleware::datair::{ExRefFunProto, CTy};

pub type ExFunTypeGetter = fn(ctx: &Context) -> FunctionType;

pub fn search_rs_lib(fname: &str) -> Option<&ExRefFunProto> {
    (*EX_RS_PROTOS).get(fname)
}

lazy_static! {
    pub static ref EX_RS_PROTOS:IndexMap<&'static str, ExRefFunProto> = indexmap! {
        "printi" => ExRefFunProto {
            name: "printi".to_owned(),
            params: vec![
                CTy::I32
            ],
            ret: CTy::I32
        },
        "prints" => ExRefFunProto {
            name: "prints".to_owned(),
            params: vec![
                CTy::CStr
            ],
            ret: CTy::CStr
        }
    };
}

