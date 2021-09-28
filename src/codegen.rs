#![allow(unused_imports)]

use inkwell::OptimizationLevel;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{
    CodeModel,
    InitializationConfig,
    RelocMode, Target, TargetMachine, FileType
};
use inkwell::types::{
    AnyTypeEnum, ArrayType, BasicType, BasicTypeEnum,
    FunctionType, IntType, PointerType, VoidType
};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallSiteValue, FloatValue,
    FunctionValue, InstructionValue, IntValue, PointerValue
};
use inkwell::AddressSpace;

use inkwell::debug_info::{
    DebugInfoBuilder, DWARFSourceLanguage, DWARFEmissionKind
};


use itertools::{Either, Itertools};
use lazy_static::lazy_static;

use indexmap::{IndexMap, indexmap, indexset};
use m6stack::{Stack, stack};

use std::convert::TryInto;
use std::error::Error;
use std::path::Path;
use std::env;
use std::rc::Rc;
use std::cell::{Ref, RefCell, RefMut};

use crate::datacg::*;
use crate::datalsp::*;
use crate::datair::*;
use crate::*;
use crate::dbi::DebugInfo;
use crate::error::{BaCErr, TrapCode};
use crate::ml_simplifier::gensym_rand;
use crate::rsclib::search_rs_lib;


pub fn codegen(config: &CompilerConfig, bamod: BaMod) -> Result<(), Box<dyn Error>> {
    let context = Context::create();

    let mut codegen
    = CodeGen::new(
        &context,
        config,
        bamod
    );

    codegen.codegen()
}


#[allow(unused)]
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,

    scope_stack: Stack<Rc<RefCell<CGScope<'ctx>>>>,
    bamod: BaMod,
    funtbl: IndexMap<BaFunKey, FunctionValue<'ctx>>,

    config: &'ctx CompilerConfig,

    dbi: DebugInfo<'ctx>,

}


#[allow(dead_code)]
impl<'ctx> CodeGen<'ctx> {
    ///////////////////////////////////////////////////////////////////////////
    //// CodeGen Init

    fn new(context: &'ctx Context, config: &'ctx CompilerConfig, bamod: BaMod) -> Self {
        let module = context.create_module(config.get_module_name().as_str());
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        /* DBI */
        let is_opt = match config.optlv {
            OptLv::Debug => false,
            _ => true
        };
        let runtime_ver = 2;

        let (dibuilder, cu) = module.create_debug_info_builder(
            true,  // allow unresolved
            DWARFSourceLanguage::C,
            &config.filename(),
            &config.dirname(),
            "BAC",
            is_opt,
            "",
            runtime_ver,
            "",
            DWARFEmissionKind::Full,
            0,
            true,
            true,
            "/",
            ""
        );

        let dbi = DebugInfo {
            cu,
            ty: None,
            dibuilder,
            lexblks: vec![]
        };

        let root_scope = Rc::new(RefCell::new(
            CGScope::new()
        ));

        Self {
            module,
            builder: context.create_builder(),
            context: &context,
            fpm,
            scope_stack: stack![root_scope],
            bamod,
            funtbl: indexmap! {},
            config,
            dbi
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    //// Scope OP

    fn push_scope(&mut self) {
        let mut child_scope = CGScope::new();
        child_scope.parent = self.scope_stack.peek().cloned();
        self.scope_stack.push(Rc::new(RefCell::new(child_scope)));
    }

    fn pop_scope(&mut self) -> Option<Rc<RefCell<CGScope<'ctx>>>> {
        self.scope_stack.pop()
    }

    fn current_scope_mut(&self) -> RefMut<CGScope<'ctx>> {
        let current_scope_rc = self.scope_stack.peek().unwrap();

        current_scope_rc.as_ref().borrow_mut()
    }

    fn current_scope(&self) -> Ref<CGScope<'ctx>> {
        let current_scope_rc = self.scope_stack.peek().unwrap();

        current_scope_rc.as_ref().borrow()
    }

    fn insert_sym_item(&mut self, key: String, sym_item: BasicValueEnum<'ctx>)
     -> Option<BasicValueEnum<'ctx>> {
        let mut current_scope_ref = self.current_scope_mut();

        current_scope_ref.symtbl.insert(key, sym_item)
    }

    fn get_sym_item(&self, key: &str) -> Option<BasicValueEnum<'ctx>> {
        let current_scope = self.current_scope();

        current_scope.get_sym_item(key)
    }

    pub fn codegen(&mut self) -> Result<(), Box<dyn Error>> {
        match self.config.target_type {
            TargetType::Bin => {
                self.codegen_bin()
            },
            _ => unimplemented!()
        }
    }

    pub fn codegen_bin(&mut self) -> Result<(), Box<dyn Error>> {
        /* Prepare DefFun CodeGen */
        self.codegen_items()?;

        let main_blk = self.begin_main();

        self.codegen_block(main_blk, &self.bamod.scope.clone())?;

        self.end_main_ok();

        self.gen_file()
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation

    fn gen_file(&self) -> Result<(), Box<dyn Error>> {
        match self.config.emit_type {
            EmitType::Obj => {
                self.emit_obj()
            },
            EmitType::LLVMIR => {
                self.emit_llvmir()
            },
            _ => todo!()
        }
    }

    fn emit_obj(&self) -> Result<(), Box<dyn Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let triple = TargetMachine::get_default_triple();
        self.module.set_triple(&triple);

        let target = Target::from_triple(&triple).unwrap();

        let machine = target.create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default
        ).unwrap();

        self.module.set_data_layout(&machine.get_target_data().get_data_layout());

        machine.write_to_file(
            &self.module,
            FileType::Object,
            &self.config.print_type.get_path().unwrap()
        )?;

        Ok(())
    }

    fn emit_llvmir(&self) -> Result<(), Box<dyn Error>> {
        if let PrintTy::File(ref path) = self.config.print_type {
            self.module
            .print_to_file(path)
            .map_err(|llvmstr| BaCErr::new_box_err(llvmstr.to_str().unwrap()))
        }
        else {
            Ok(self.module.print_to_stderr())
        }

    }


    ///////////////////////////////////////////////////////////////////////////
    //// Codegen Items

    fn codegen_items(&mut self) -> Result<(), Box<dyn Error>> {
        let bascope_rc = self.bamod.scope.clone();
        let bascope_ref = bascope_rc.as_ref().borrow();

        for bastmt in bascope_ref.stmts.iter() {
            match &bastmt {
                BaStmt::DefFun(defn) => {
                    self.codegen_defn(defn)?;
                },
                _ => {}
            }
        }

        Ok(())
    }



    ///////////////////////////////////////////////////////////////////////////
    //// Entry point codegen

    fn begin_main(&self) -> BasicBlock<'ctx> {
        let i64_type = self.context.i64_type();
        let fn_main_t = i64_type.fn_type(&[], false);
        let fn_main = self.module.add_function("main", fn_main_t, None);
        let blk_main = self.context.append_basic_block(fn_main, "mainblk");
        self.builder.position_at_end(blk_main);

        blk_main
    }

    fn end_main(&self, rtn: IntValue<'ctx>) {
        self.builder.build_return(Some(&rtn));
        let main_fn = self.module.get_function("main").unwrap();

        if main_fn.verify(true) {
            if self.config.optlv != OptLv::Debug {
                self.fpm.run_on(&main_fn);
            }
        }
    }

    fn end_main_ok(&self) {
        let i64_type = self.context.i64_type();

        self.end_main(i64_type.const_zero())
    }


    ///////////////////////////////////////////////////////////////////////////
    //// BaStmts codegen

    pub fn codegen_stmts(&mut self, stmts: &Vec<BaStmt>) -> Result<(), Box<dyn Error>> {
        for stmt in stmts.iter() {
            match stmt {
                BaStmt::Declare(dec) => {
                    self.codegen_declare(dec)?;
                },
                BaStmt::FunCall(funcall) => {
                    self.codegen_funcall(funcall)?;
                },
                BaStmt::Block(_block) => {
                    todo!()
                },
                BaStmt::DefFun(_defn) => {
                    // skip
                    // self.codegen_defn(defn)?;
                }
            }
        }

        Ok(())
    }


    ///////////////////////////////////////////////////////////////////////////
    //// BaStmt codegen

    fn codegen_defn(&mut self, defn: &BaDefFun) -> Result<(), Box<dyn Error>> {
        let ret_ty = self.baty_to_retty(&defn.hdr.ret);
        let params_ty = defn.hdr.params
        .iter()
        .map(|param| self.baty_to_basicty(&param.ty).unwrap())
        .collect_vec();

        let fn_xx_t = if let Either::Left(basic_ty) = ret_ty {
            basic_ty.fn_type(&params_ty[..], false)
        }
        else if let Either::Right(void_ty) = ret_ty {
            void_ty.fn_type(&params_ty[..], false)
        }
        else { unreachable!() };

        let fn_xx
        = self.module.add_function(&defn.hdr.name(), fn_xx_t, None);

        // 在simplifier中已经检查过了
        self.funtbl.insert(defn.hdr.as_key(), fn_xx.clone());

        let fn_blk
        = self.context.append_basic_block(
            fn_xx,
            &format!("{}_block", defn.hdr.name())
            );

        /* Build Fn Block */
        self.push_scope();

        for (i, param) in defn.hdr.params.iter().enumerate() {
            let val = fn_xx.get_nth_param(i as u32).unwrap();

            self.insert_sym_item(
                param.formal.to_owned(),
                val
            );
        }

        if let Some(retval) = self._codegen_block_0(fn_blk, &defn.body)? {
            self.builder.build_return(Some(&retval));
        }
        else {
            self.builder.build_return(None);
        }

        self.pop_scope();

        Ok(())
    }

    fn _codegen_block_0(&mut self, blk: BasicBlock, scope: &Rc<RefCell<BaScope>>)
    -> Result<Option<BasicValueEnum<'ctx>>, Box<dyn Error>>
    {
        self.builder.position_at_end(blk);

        let scope_ref = scope.as_ref().borrow();

        self.codegen_stmts(&scope_ref.stmts)?;

        Ok(if let Some(ref prival) = scope_ref.tailval {
            Some(self.pri2value(prival))
        }
        else {
            None
        })
    }

    fn codegen_block(&mut self, blk: BasicBlock, scope: &Rc<RefCell<BaScope>>)
     -> Result<(), Box<dyn Error>>
    {
        self.push_scope();
        self._codegen_block_0(blk, scope)?;
        self.pop_scope();

        Ok(())
    }

    fn baty_to_basicty(&self, baty: &BaType) -> Option<BasicTypeEnum<'ctx>> {
        if let BaType::VoidUnit = baty {
            return None
        }

        Some(match &baty {
            BaType::Float => BasicTypeEnum::<'ctx>::FloatType(
                self.context.f64_type()
            ),
            BaType::I64  => BasicTypeEnum::<'ctx>::IntType(
                self.context.i64_type()
            ),
            BaType::Int => BasicTypeEnum::<'ctx>::IntType(
                self.context.i32_type()
            ),
            BaType::Str => BasicTypeEnum::<'ctx>::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic)
            ),
            BaType::USize => {
                if cfg!(target_pointer_width="64") {
                    BasicTypeEnum::<'ctx>::IntType(
                        self.context.i64_type()
                    )
                }
                else if cfg!(target_pointer_width="32") {
                    BasicTypeEnum::<'ctx>::IntType(
                        self.context.i32_type()
                    )
                }
                else {
                    unimplemented!()
                }
            },
            BaType::U8 => {
                BasicTypeEnum::<'ctx>::IntType(
                    self.context.i8_type()
                )
            },
            BaType::Customized(_typeid_rc) => {
                todo!()
            },
            BaType::ExRefFunProto(_exref_funproto) => {
                todo!()
            },
            BaType::VoidUnit => unreachable!()
        })
    }

    fn baty_to_retty(&self, baty: &BaType) -> Either<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match &baty {
            BaType::VoidUnit => Either::Right(
                self.context.void_type()
            ),
            _ => {
                Either::Left(self.baty_to_basicty(baty).unwrap())
            }
        }
    }

    fn codegen_declare(&mut self, dec: &BaDeclare) -> Result<(), Box<dyn Error>> {
        let id = &dec.name;
        let res;

        match &dec.value {
            BaDecVal::PriVal(prival) => {
                res = self.codegen_prival(prival)?;
            },
            BaDecVal::FunCall(funcall) => {
                let funrtn_callsite = self.codegen_funcall(funcall)?;
                let funrtn_either = funrtn_callsite.try_as_basic_value();

                if funrtn_either.is_right() {
                    return Err(
                        BaCErr::new_box_err(
                            format!("get None from {:?}", funcall).as_str()
                        )
                    )
                }
                else {
                    res = funrtn_either.left().unwrap();
                }
            },
            BaDecVal::TwoAddr(bop, fstprival, sndprival) => {
                res = self.codegen_2addrexpr(bop, fstprival, sndprival)?;
            },
        }

        self.insert_sym_item(id.name.to_string(), res);

        Ok(())
    }

    fn codegen_prival(&mut self, prival: &BaPriVal)
    -> Result<BasicValueEnum<'ctx>, Box<dyn Error>>
    {
        let res: BasicValueEnum;

        match prival {
            BaPriVal::Lit(lit) => {
                res = self.lit2value(lit);
            },
            BaPriVal::Id(valid) => {
                if let Some(valsymptr) = self.get_sym_item(&valid.name) {
                    res = valsymptr.clone();
                }
                else {
                    return Err(
                        TrapCode::UnresolvedSymbol(valid).emit_box_err()
                    )
                }
            },
        }

        Ok(res)
    }

    fn codegen_funcall(&mut self, funcall: &BaFunCall)
    -> Result<CallSiteValue<'ctx>, Box<dyn Error>>
    {
        // search fun ref
        let fid = &funcall.name;

        let fnval;
        match fid.splid {
            Some(BaSplId::RS) => {
                let funtype;
                if let Some(proto) = search_rs_lib(&fid.name) {
                    funtype = self.exproto_to_funtype(proto);
                }
                else {
                    unreachable!()
                }

                if let Some(_fnval) = self.module.get_function(&fid.name) {
                    fnval = _fnval;
                }
                else {
                    fnval = self.module.add_function(
                        &fid.name,
                        funtype,
                        Some(Linkage::External)
                    );
                }
            },
            None => {
                let funkey = funcall.get_fun_key()?;

                fnval = if let Some(fn_xx) = self.funtbl.get(&funkey) {
                    fn_xx.clone()
                }
                else {
                    unreachable!()
                };
            }
        }

        // codegen fun call args
        let args_vec = self.codegen_args(&funcall.args[..])?;

        let rtn = self.builder.build_call(
            fnval,
            &args_vec[..],
            format!("call_{}", &fid.name).as_str()
        );

        Ok(rtn)
    }

    fn funkey_ret_to_funtype(&self, funkey: &BaFunKey, ret: &BaType) -> FunctionType<'ctx> {
        let params = funkey.1.iter().map(|baty| {
            self.baty_to_basicty(baty).unwrap()
        }).collect_vec();

        match self.baty_to_retty(ret) {
            Either::Left(ty) => {
                ty.fn_type(&params[..], false)
            },
            Either::Right(void_t) => {
                void_t.fn_type(&params[..], false)
            }
        }
    }

    fn codegen_args(&mut self, prival_iter: &[BaPriVal])
    -> Result<Vec<BasicValueEnum<'ctx>>, Box<dyn Error>>
    {
        let mut res = vec![];

        for prival in prival_iter {
            match prival {
                BaPriVal::Lit(lit) => {
                    res.push(self.lit2value(lit))
                },
                BaPriVal::Id(id) => {
                    if let Some(ptrval) = self.get_sym_item(&id.name) {
                        res.push(
                            ptrval.clone()
                        );
                    }
                    else {
                        return Err(
                            TrapCode::UnresolvedSymbol(id).emit_box_err()
                        )
                    }
                }
            }
        }

        Ok(res)
    }

    fn codegen_2addrexpr(&mut self, bop: &BaBOp, fstpri: &BaPriVal, sndpri: &BaPriVal)
    -> Result<BasicValueEnum<'ctx>, Box<dyn Error>>
    {
        let fst_bv = self.codegen_prival(fstpri)?;
        let snd_bv = self.codegen_prival(sndpri)?;

        let res: BasicValueEnum;

        match bop {
            &BaBOp::Add => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        res = self.builder.build_int_add(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    BasicValueEnum::FloatValue(_) => {
                        res = self.builder.build_float_add(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
            &BaBOp::Sub => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        res = self.builder.build_int_sub(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        ).into()
                    },
                    BasicValueEnum::FloatValue(_) => {
                        res = self.builder.build_float_sub(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        ).into()
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
            &BaBOp::Mul => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        res = self.builder.build_int_mul(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    BasicValueEnum::FloatValue(_) => {
                        res = self.builder.build_float_mul(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            }
            &BaBOp::Div => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        res = self.builder.build_int_signed_div(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    BasicValueEnum::FloatValue(_) => {
                        res = self.builder.build_float_div(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }

            },
            &BaBOp::Mod => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        res = self.builder.build_int_signed_rem(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    BasicValueEnum::FloatValue(_) => {
                        res = self.builder.build_float_rem(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        ).into();
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
        }

        Ok(res)
    }



    ///////////////////////////////////////////////////////////////////////////
    //// Basic Type Cast

    fn try_as_float_val(&self, vale: BasicValueEnum<'ctx>)
    -> Result<FloatValue<'ctx>, Box<dyn Error>>
    {
        match vale {
            BasicValueEnum::FloatValue(v) => {
                Ok(v)
            },
            _ => {
                unreachable!()
            }
        }
    }

    fn try_as_int_val(&self, vale: BasicValueEnum<'ctx>)
    -> Result<IntValue<'ctx>, Box<dyn Error>>
    {
        match vale {
            BasicValueEnum::IntValue(v) => {
                Ok(v)
            },
            _ => {
                unreachable!("try as int {:?}", vale)
            }
        }
    }

    fn lit2value(&self, lit: &BaLit) -> BasicValueEnum<'ctx> {
        let res;

        match lit {
            BaLit::I32(i32val) => {
                let i32_t = self.context.i32_type();
                res = i32_t.const_int(i32val.val as u64, false).into();
            },
            BaLit::Str(bastr) => {
                let strval = &bastr.val;
                let i8_t = self.context.i8_type();
                let i64_t = self.context.i64_type();

                let mut cchars
                = strval
                .as_bytes()
                .into_iter()
                .map(|x| i8_t.const_int(*x as u64, false))
                .collect_vec();

                cchars.push(i8_t.const_zero());

                let cchars_len
                = i64_t.const_int(
                    (strval.len() + 1 as usize).try_into().unwrap(),
                    false
                );

                // 栈上分配
                let cchars_ptr
                = self.builder.build_array_alloca(i8_t, cchars_len, &bastr.tag_str());
                // // 堆上分配
                // let cchars_ptr
                // = self.builder.build_array_malloc(i8_strarr_t, cchars_len, "buildstr")?;
                for (i, cchar_int) in cchars.into_iter().enumerate() {
                    let idx = i64_t.const_int(i as u64, false);
                    let ptr
                    = unsafe { self.builder.build_gep(cchars_ptr, &[idx.into()], "") };
                    self.builder.build_store(ptr, cchar_int);
                }

                res = cchars_ptr.into();
            },
            _ => unreachable!()
        }

        res
    }

    fn pri2value(&self, prival: &BaPriVal) -> BasicValueEnum<'ctx> {
        match &prival {
            BaPriVal::Lit(lit) => {
                self.lit2value(&lit)
            },
            BaPriVal::Id(id) => {
                self.get_sym_item(&id.name).unwrap()
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Foreign Type Cast

    fn exproto_to_funtype(&self, proto: &ExRefFunProto) -> FunctionType<'ctx> {
        let params = proto.params.iter().map(|param| {
            self.exty_to_basicty(param).unwrap()
        }).collect_vec();

        match self.exty_to_retty(&proto.ret) {
            Either::Left(ty) => {
                ty.fn_type(&params[..], false)
            },
            Either::Right(void_t) => {
                void_t.fn_type(&params[..], false)
            }
        }
    }

    fn exty_to_retty(&self, ty: &ExRefType) -> Either<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match &ty {
            ExRefType::F64  => Either::Left(BasicTypeEnum::<'ctx>::FloatType(
                self.context.f64_type()
            )),
            ExRefType::I64  => Either::Left(BasicTypeEnum::<'ctx>::IntType(
                self.context.i64_type()
            )),
            ExRefType::I32  => Either::Left(BasicTypeEnum::<'ctx>::IntType(
                self.context.i32_type()
            )),
            ExRefType::Void => Either::Right(self.context.void_type()),
            ExRefType::Str => Either::Left(BasicTypeEnum::<'ctx>::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic)
            ))
        }
    }

    fn exty_to_basicty(&self, ty: &ExRefType) -> Option<BasicTypeEnum<'ctx>> {
        match self.exty_to_retty(ty) {
            Either::Left(basic) => Some(basic),
            Either::Right(_) => None
        }
    }


}



////////////////////////////////////////////////////////////////////////////////
//// Helper Function







#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use crate::lexer::{tokenize, trim_tokens, SrcFileInfo};
    use crate::manual_parser::Parser;
    use crate::{CompilerConfig, VERBOSE, VerboseLv};

    use crate::ml_simplifier::MLSimplifier;

    use super::codegen;

    #[test]
    fn test_algb_op() {
        println!("{}", 1f64+(3.0/2.0) -2f64);

        println!("-4: {}", (0-4) as u64);
        println!("-4: {}", (0-4) as u64 as i32);

    }

    #[test]
    fn test_codegen() {
        let file = SrcFileInfo::new(PathBuf::from("./examples/exp1.ba")).unwrap();
        let config = CompilerConfig {
            file,
            optlv: crate::OptLv::Debug,
            target_type: crate::TargetType::Bin,
            emit_type: crate::EmitType::LLVMIR,
            print_type: crate::utils::PrintTy::StdErr
        };

        let tokens = tokenize(&config.file);
        let tokens = trim_tokens(tokens);

        let mut parser = Parser::new(tokens);
        let ml = parser.parse().unwrap();

        let mlslf = MLSimplifier::new(ml);
        let bin = mlslf.simplify();

        codegen(&config, bin).unwrap();
    }

}
