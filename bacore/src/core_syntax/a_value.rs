use std::error::Error;

use inkwell::values::{BasicValueEnum, StructValue};

use super::ConcreteTypeAnno;


#[derive(Clone)]
pub struct AValue<'ctx> {
    pub type_anno: ConcreteTypeAnno,
    pub vm_val: BasicValueEnum<'ctx>
}

// impl<'ctx> TryFrom<AValue<'ctx>> for StructValue<'ctx> {
//     type Error = Box<dyn Error>;

//     fn try_from(value: AValue<'ctx>) -> Result<Self, Self::Error> {
//         if let ConcreteTypeAnno::Struct(ref struct_name, ref addr_mode) = value.type_anno
//         {
//             let astruct = cctx.form_struct_map.get(struct_name).unwrap();

//             if let Some(field_index) = astruct.index_of_field(field_name) {
//                 builder.build_extract_value(avalue.val.in, field_index as u32, "");
//             }

//         }
//     }
// }