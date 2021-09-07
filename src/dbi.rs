
use inkwell::debug_info::{
    DICompileUnit, DIType, DIScope, DebugInfoBuilder
};



#[allow(unused)]
pub struct DebugInfo<'ctx> {
    // Compile Unit
    pub(crate) cu: DICompileUnit<'ctx>,
    pub(crate) ty: Option<DIType<'ctx>>,
    pub(crate) lexblks: Vec<DIScope<'ctx>>,
    pub(crate) dibuilder: DebugInfoBuilder<'ctx>
}

impl<'ctx> DebugInfo<'ctx> {
    // fn emit_loc
}
