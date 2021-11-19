use inkwell::{
    builder::Builder,
    context::Context,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::FunctionValue,
};

use super::CompileContext;
use crate::*;

fn init_fn_definition<'ctx>(
    ans: &mut ANS<'ctx>,
    fn_val: FunctionValue<'ctx>,
) -> Builder<'ctx> {
    let fn_block = ans.ctx.vmctx.append_basic_block(fn_val, "");
    let builder = ans.ctx.vmctx.create_builder();
    builder.position_at_end(fn_block);

    builder
}

pub(crate) fn load_primitive_function<'ctx>(
    ans: &mut ANS<'ctx>,
) -> Result<(), Box<dyn Error>> {
    /* ADD */
    let fn_val =
    add_primitive_function!(ans, "add" [ "usize", "usize" ] -> "usize" | inline)?;

    let builder = init_fn_definition(ans, fn_val);
    builder.build_int_add(
        fn_val.get_nth_param(0).unwrap().into_int_value(),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );


    let fn_val = add_primitive_function!(ans, "add" [ "u32", "u64" ] -> "u64" | inline)?;
    let builder = init_fn_definition(ans, fn_val);
    builder.build_int_add(
        fn_val
            .get_nth_param(0)
            .unwrap()
            .into_int_value()
            .const_cast(ans.ctx.vmctx.i64_type(), true),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    let fn_val = add_primitive_function!(ans, "add" [ "float", "i64" ] -> "float" | inline)?;
    let builder = init_fn_definition(ans, fn_val);

    let arg1st = fn_val.get_nth_param(0).unwrap().into_float_value();
    let arg2nd = builder
        .build_bitcast(
            fn_val.get_nth_param(1).unwrap(),
            ans.ctx.vmctx.f64_type(),
            "",
        )
        .into_float_value();
    builder.build_float_add(arg1st, arg2nd, "");


    let fn_val = add_primitive_function!(ans, "add" [ "i64", "float" ] -> "float" | inline)?;
    let builder = init_fn_definition(ans, fn_val);

    builder.build_int_add(
        fn_val
            .get_nth_param(0)
            .unwrap()
            .into_int_value()
            .const_cast(ans.ctx.vmctx.i64_type(), true),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    /* SUB */
    let fn_val = add_primitive_function!(ans, "sub" [ "usize", "usize" ] -> "usize" | inline)?;
    let builder = init_fn_definition(ans, fn_val);

    builder.build_int_sub(
        fn_val.get_nth_param(0).unwrap().into_int_value(),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );



    Ok(())
}

pub(crate) fn build_fn_type<'ans>(
    vmctx: &'ans Context,
    param_types: &[BasicMetadataTypeEnum<'ans>],
    ret: &str,
) -> FunctionType<'ans> {
    match ret {
        "usize" | "u64" | "i64" => {
            vmctx.i64_type().fn_type(param_types, false)
        }
        "u32" | "i32" => vmctx.i32_type().fn_type(param_types, false),
        "u8" | "i8" => vmctx.i8_type().fn_type(param_types, false),
        "bool" => vmctx.bool_type().fn_type(param_types, false),
        "void" => vmctx.void_type().fn_type(param_types, false),

        _ => unreachable!(ret),
    }
}

#[macro_export]
macro_rules! add_primitive_function {
    ($ans:ident, $base_name:literal [ $($arg:literal),* ] -> $ret:literal | $fn_mode:ident ) => {
        {
            use crate::core_syntax::{
                type_anno::{
                    ConcreteTypeAnno,
                },
                a_fn::{ AFn, ConcreteParam }
            };
            use itertools::Itertools;
            use inkwell::module::Linkage;
            use lisparser::data::*;

            let mut collected_args = Vec::<&str>::new();

            $(
                collected_args.push($arg);
            )*

            let params = collected_args
            .into_iter()
            .map_into()
            .collect::<Vec<ConcreteTypeAnno>>()
            .into_iter()
            .enumerate()
            .map(|(i, type_anno)| ConcreteParam { formal: i.to_string(), type_anno })
            .collect_vec();

            let ret = Some(ConcreteTypeAnno::from($ret));

            let afn = AFn {
                name: $base_name.to_owned(),
                params,
                ret,
                body: ListData::nil(),
            };

            let linkage = match stringify!($fn_mode) {
                "inline" => Some(Linkage::LinkOnceODR),
                "default" => None,
                _ => unreachable!(stringify!($fn_mode))
            };

            $ans.compile_declare(&afn, linkage)
        }
    };
}
