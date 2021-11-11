use convert_case::{Case, Casing};
use itertools::Itertools;

use super::type_anno::ConcreteTypeAnno;




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

    name.to_owned() + "@" + &overload_postfix
}


pub(crate) fn rename_fn_alias(name: &str) -> String {
    match name {
        "+" => "add",
        "-" => "sub",
        "*" => "mul",
        "=" => "eq",
        "/" => "div",
        "." => "attr",
        "->" => "deref-attr",
        _ => name
    }.to_owned()
}