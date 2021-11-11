use inkwell::{context::Context, types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType}};

use super::CompileContext;
use crate::*;



pub(crate) fn load_primitive_function<'ctx>(
    ctx: &'ctx mut CompileContext<'ctx>,
) -> Result<(), Box<dyn Error>> {
    /* ADD */
    let fn_val = add_primitive_function!(ctx, "add" [ "usize", "usize" ] -> "usize" | inline);
    let fn_block = ctx.vmctx.append_basic_block(fn_val, "");
    let loc_builder = ctx.vmctx.create_builder();
    loc_builder.position_at_end(fn_block);
    loc_builder.build_int_add(
        fn_val.get_nth_param(0).unwrap().into_int_value(),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    let fn_val =
        add_primitive_function!(ctx, "add" [ "u32", "u64" ] -> "u64" | inline);
    let fn_block = ctx.vmctx.append_basic_block(fn_val, "");
    let loc_builder = ctx.vmctx.create_builder();
    loc_builder.position_at_end(fn_block);
    loc_builder.build_int_add(
        fn_val
            .get_nth_param(0)
            .unwrap()
            .into_int_value()
            .const_cast(ctx.vmctx.i64_type(), true),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    let fn_val = add_primitive_function!(ctx, "add" [ "float", "i64" ] -> "float" | inline);
    let fn_block = ctx.vmctx.append_basic_block(fn_val, "");
    let loc_builder = ctx.vmctx.create_builder();
    loc_builder.position_at_end(fn_block);
    let arg1st = fn_val.get_nth_param(0).unwrap().into_float_value();
    let arg2nd = loc_builder
        .build_bitcast(
            fn_val.get_nth_param(1).unwrap(),
            ctx.vmctx.f64_type(),
            "",
        )
        .into_float_value();
    loc_builder.build_float_add(arg1st, arg2nd, "");

    let fn_val = add_primitive_function!(ctx, "add" [ "i64", "float" ] -> "float" | inline);
    let fn_block = ctx.vmctx.append_basic_block(fn_val, "");
    let loc_builder = ctx.vmctx.create_builder();
    loc_builder.position_at_end(fn_block);
    loc_builder.build_int_add(
        fn_val
            .get_nth_param(0)
            .unwrap()
            .into_int_value()
            .const_cast(ctx.vmctx.i64_type(), true),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    /* SUB */
    let fn_val = add_primitive_function!(ctx, "sub" [ "usize", "usize" ] -> "usize" | inline);
    let fn_block = ctx.vmctx.append_basic_block(fn_val, "");
    let loc_builder = ctx.vmctx.create_builder();
    loc_builder.position_at_end(fn_block);
    loc_builder.build_int_sub(
        fn_val.get_nth_param(0).unwrap().into_int_value(),
        fn_val.get_nth_param(1).unwrap().into_int_value(),
        "",
    );

    Ok(())
}




pub(crate) fn build_fn_type<'ctx>(
    vmctx: &'ctx Context,
    param_types: &[BasicMetadataTypeEnum<'ctx>],
    ret: &str,
) -> FunctionType<'ctx> {
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
    ($ctx:ident, $base_name:literal [ $($arg:literal),* ] -> $ret:literal | $fn_mode:ident ) => {
        {
            use crate::core_syntax::{
                type_anno::{
                    compile_concrete_types,
                    ConcreteTypeAnno,
                },
                hardcode_fns::build_fn_type,
                name_mangling:: {
                    concat_overload_name,
                    NameConcatStyle
                }
            };
            use itertools::Itertools;
            use inkwell::module::Linkage;

            let mut collected_args = Vec::<&str>::new();

            $(
                collected_args.push($arg);
            )*

            let concrete_types = collected_args
            .into_iter()
            .map_into()
            .collect::<Vec<ConcreteTypeAnno>>();

            let fn_name = concat_overload_name($base_name, &concrete_types[..], NameConcatStyle::Fn);

            let args_vm_t_vec = compile_concrete_types($ctx, &concrete_types[..])?
                .into_iter()
                .map_into()
                .collect_vec();

            let fn_t = build_fn_type(&$ctx.vmctx, &args_vm_t_vec[..], $ret);

            let linkage = match stringify!($fn_mode) {
                "inline" => Some(Linkage::LinkOnceODR),
                "default" => None,
                _ => unreachable!(stringify!($fn_mode))
            };

            $ctx.vmmod.add_function(&fn_name, fn_t, linkage)
        }
    };
}

