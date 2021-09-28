//! Codegen Data

use std::{cell::RefCell, rc::Rc};

use indexmap::{
    IndexMap, indexmap
};
use inkwell::values::BasicValueEnum;



///////////////////////////////////////////////////////////////////////////
//// CodeGen Scope

pub type CGSymTbl<'ctx> = IndexMap<String, BasicValueEnum<'ctx>>;

#[derive(Debug, Clone)]
pub struct CGScope<'ctx> {
    pub(crate) parent: Option<Rc<RefCell<Self>>>,
    pub(crate) symtbl: CGSymTbl<'ctx>
}

impl <'ctx> CGScope<'ctx> {
    pub fn new() -> Self {
        Self {
            parent: None,
            symtbl: indexmap! {}
        }
    }

    pub fn get_sym_item(&self, key: &str) -> Option<BasicValueEnum<'ctx>> {
        if let Some(item) = self.symtbl.get(key) {
            Some(item.clone())
        }
        else if let Some(paren) = &self.parent {
            paren.as_ref().borrow().get_sym_item(key)
        }
        else {
            None
        }
    }
}

