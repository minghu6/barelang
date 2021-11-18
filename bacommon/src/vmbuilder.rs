use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::Context,
    types::BasicTypeEnum, values::PointerValue,
};

pub fn builder_position_at_before<'ctx>(
    builder: &mut Builder<'ctx>,
    entry: BasicBlock<'ctx>,
) {
    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }
}

/// Creates a new stack allocation instruction in the entry block of the function.
pub fn create_entry_block_alloca<'ctx>(
    context: &'ctx Context,
    entry: BasicBlock<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = context.create_builder();

    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }

    builder.build_alloca(ty, name)
}
