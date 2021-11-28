use convert_case::{Case, Casing};
use itertools::Itertools;

use super::type_anno::ConcreteTypeAnno;
use super::r#const::*;



pub(crate) enum NameConcatStyle {
    Fn,
    Struct,
}

pub(crate) fn concat_overload_name(
    name: &str,
    concrete_types: &[ConcreteTypeAnno],
    concat_style: NameConcatStyle,
) -> String {
    let overload_postfix = match concat_style {
        NameConcatStyle::Fn => concrete_types
            .iter()
            .map(|type_anno| type_anno.to_string().to_case(Case::Snake))
            .join("_"),
        NameConcatStyle::Struct => concrete_types
            .iter()
            .map(|anno| anno.to_string().to_case(Case::Camel))
            .join(""),
    };

    name.to_owned() + "_" + &overload_postfix
}


pub(crate) fn rename_fn_alias(name: &str) -> String {
    match name {
        "+" => FORM_ADD,
        "-" => FORM_SUB,
        "*" => FORM_MUL,
        "=" => FORM_EQ,
        "/" => FORM_DIV,
        "." => FORM_ATTR,
        "->" => FORM_DEREF_ATTR,
        ">" => FORM_GT,
        ">=" => FORM_GE,
        _ => name
    }.to_owned()
}
