use inkwell::{
    builder::Builder,
    context::Context,
    module::Linkage,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::FunctionValue,
    AddressSpace,
};

use super::CompileContext;
use crate::*;

pub(crate) fn init_fn_definition<'ctx>(
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
    let const_char_ptr_t =
        ans.ctx.vmctx.i8_type().ptr_type(AddressSpace::Const);
    let int_t = ans.ctx.vmctx.i32_type();

    /* Add C printf */
    let fn_cprintf_t = int_t.fn_type(&[const_char_ptr_t.into()], true);
    ans.ctx.vmmod.add_function(
        "printf",
        fn_cprintf_t,
        Some(Linkage::External),
    );

    Ok(())
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
