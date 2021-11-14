#![allow(unused_imports)]

use bacommon::config::{CompilerConfig, EmitType, OptLv, PrintTy, TargetType, usize_len};
use bacommon::lexer::SrcFileInfo;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::{Context, ContextRef};
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target,
    TargetMachine,
};
use inkwell::types::{
    AnyTypeEnum, ArrayType, BasicType, BasicTypeEnum, FunctionType, IntType,
    PointerType, VoidType,
};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallSiteValue, FloatValue, FunctionValue,
    InstructionValue, IntValue, PointerValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};

use inkwell::debug_info::{
    DWARFEmissionKind, DWARFSourceLanguage, DebugInfoBuilder,
};

use itertools::{Either, Itertools};
use lazy_static::lazy_static;

use indexmap::{indexmap, indexset, IndexMap};
use m6stack::{stack, Stack};

use std::cell::{Ref, RefCell, RefMut};
use std::convert::{TryFrom, TryInto};
use std::env;
use std::error::Error;
use std::path::Path;
use std::rc::Rc;

use crate::backend::datacg::*;
use crate::dbi::DebugInfo;
use crate::error::{BaCErr, TrapCode};
use crate::frontend::datalsp::*;
use crate::middleware::datair::*;
use crate::middleware::ml_simplifier::gensym_rand;
use crate::rsclib::search_rs_lib;
use crate::*;

pub fn codegen(
    config: &CompilerConfig,
    src: SrcFileInfo,
    bamod: BaMod,
) -> Result<(), Box<dyn Error>> {
    let context = Context::create();

    let mut codegen = CodeGen::new(&context, config, src, bamod);

    codegen.codegen()
}

#[allow(unused)]
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,

    scope_stack: Stack<Rc<RefCell<CGScope<'ctx>>>>,
    nested_funval_stack: Stack<FunctionValue<'ctx>>,
    bamod: BaMod,
    funtbl: IndexMap<BaFunKey, CGFn<'ctx>>,

    config: &'ctx CompilerConfig,

    dbi: DebugInfo<'ctx>,
}

#[allow(dead_code)]
impl<'ctx> CodeGen<'ctx> {
    ///////////////////////////////////////////////////////////////////////////
    //// CodeGen Init

    fn new(
        context: &'ctx Context,
        config: &'ctx CompilerConfig,
        src: SrcFileInfo,
        bamod: BaMod,
    ) -> Self {
        let module = context.create_module("");
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_tail_call_elimination_pass();

        fpm.initialize();

        /* DBI */
        let is_opt = match config.optlv {
            OptLv::Debug => false,
            _ => true,
        };
        let runtime_ver = 2;

        let (dibuilder, cu) = module.create_debug_info_builder(
            true, // allow unresolved
            DWARFSourceLanguage::C,
            &src.filename(),
            &src.dirname(),
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
            "",
        );

        let dbi = DebugInfo {
            cu,
            ty: None,
            dibuilder,
            lexblks: vec![],
        };

        let root_scope = Rc::new(RefCell::new(CGScope::new()));

        Self {
            module,
            builder: context.create_builder(),
            context: &context,
            fpm,
            scope_stack: stack![root_scope],
            bamod,
            funtbl: indexmap! {},
            config,
            dbi,
            nested_funval_stack: stack![],
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

    fn insert_sym_item(
        &mut self,
        key: String,
        sym_item: CGValue<'ctx>,
    ) -> Option<CGValue<'ctx>> {
        let mut current_scope_ref = self.current_scope_mut();

        current_scope_ref.symtbl.insert(key, sym_item)
    }

    fn get_sym_item(&self, key: &str) -> Option<CGValue<'ctx>> {
        let current_scope = self.current_scope();

        current_scope.get_sym_item(key)
    }

    pub fn codegen(&mut self) -> Result<(), Box<dyn Error>> {
        match self.config.target_type {
            TargetType::Bin => self.codegen_bin(),
            _ => unimplemented!(),
        }
    }

    pub fn codegen_bin(&mut self) -> Result<(), Box<dyn Error>> {
        /* Prepare DefFun CodeGen */
        self.codegen_items()?;

        // dbg!(self.funtbl.keys());

        self.begin_main();

        self.codegen_block(&self.bamod.scope.clone())?;

        self.end_main_ok();

        self.gen_file()
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Target Generation

    fn gen_file(&self) -> Result<(), Box<dyn Error>> {
        match self.config.emit_type {
            EmitType::Obj => self.emit_obj(),
            EmitType::LLVMIR => self.emit_llvmir(),
            _ => todo!(),
        }
    }

    fn emit_obj(&self) -> Result<(), Box<dyn Error>> {
        Target::initialize_native(&InitializationConfig::default())?;

        let triple = TargetMachine::get_default_triple();
        self.module.set_triple(&triple);

        let target = Target::from_triple(&triple).unwrap();

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                self.config.optlv.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        self.module
            .set_data_layout(&machine.get_target_data().get_data_layout());

        machine.write_to_file(
            &self.module,
            FileType::Object,
            &self.config.print_type.get_path().unwrap(),
        )?;

        Ok(())
    }

    fn emit_llvmir(&self) -> Result<(), Box<dyn Error>> {
        if let PrintTy::File(ref path) = self.config.print_type {
            self.module.print_to_file(path).map_err(|llvmstr| {
                BaCErr::new_box_err(llvmstr.to_str().unwrap())
            })
        } else {
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
                    self.codegen_defn_hdr(defn)?;
                }
                _ => {}
            }
        }

        /* Two pass scan */
        for bastmt in bascope_ref.stmts.iter() {
            match &bastmt {
                BaStmt::DefFun(defn) => {
                    self.codegen_defn(defn)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn codegen_defn_hdr(
        &mut self,
        defn: &BaDefFun,
    ) -> Result<(), Box<dyn Error>> {
        let ret_ty = self.baty_to_retty(&defn.hdr.ret);
        let params_ty = defn
            .hdr
            .params
            .iter()
            .map(|param| self.baty_to_basicty(&param.ty).unwrap().into())
            .collect_vec();

        let fn_xx_t = if let Either::Left(basic_ty) = ret_ty {
            basic_ty.fn_type(&params_ty[..], false)
        } else if let Either::Right(void_ty) = ret_ty {
            void_ty.fn_type(&params_ty[..], false)
        } else {
            unreachable!()
        };

        let fn_xx = self.module.add_function(&defn.hdr.name(), fn_xx_t, None);

        let cgfn = CGFn::from((defn.hdr.clone(), fn_xx));

        // 在simplifier中已经检查过了
        self.funtbl.insert(defn.hdr.as_key(), cgfn);

        Ok(())
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Entry point codegen

    fn begin_main(&mut self) -> BasicBlock<'ctx> {
        let i64_type = self.context.i64_type();
        let fn_main_t = i64_type.fn_type(&[], false);
        let fn_main = self.module.add_function("main", fn_main_t, None);
        let blk_main = self.context.append_basic_block(fn_main, "mainblk");
        self.builder.position_at_end(blk_main);
        self.nested_funval_stack.push(fn_main);

        blk_main
    }

    fn end_main(&mut self, rtn: IntValue<'ctx>) {
        self.builder.build_return(Some(&rtn));
        let main_fn = self.module.get_function("main").unwrap();

        if main_fn.verify(true) {
            if self.config.optlv != OptLv::Debug {
                self.fpm.run_on(&main_fn);
            }
        }

        self.nested_funval_stack.pop();
    }

    fn end_main_ok(&mut self) {
        let i64_type = self.context.i64_type();

        self.end_main(i64_type.const_zero())
    }

    ///////////////////////////////////////////////////////////////////////////
    //// BaStmts codegen

    pub fn codegen_stmts(
        &mut self,
        stmts: &Vec<BaStmt>,
    ) -> Result<(), Box<dyn Error>> {
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
                BaStmt::IterBlock(iterblock) => {
                    self.codegen_iter_block(iterblock)?;
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
        let cgfn = self.funtbl.get(&defn.hdr.as_key()).unwrap().clone();
        let fn_xx = cgfn.val.clone();
        drop(cgfn);

        let fn_blk = self.context.append_basic_block(
            fn_xx.clone(),
            &format!("{}_block", defn.hdr.name()),
        );

        /* Build Fn Block */
        self.push_scope();

        for (i, param) in defn.hdr.params.iter().enumerate() {
            let bv = fn_xx.get_nth_param(i as u32).unwrap();
            let ty = defn.hdr.params[i].ty.clone();

            let cgv = CGValue::try_from((ty, bv))?;

            self.insert_sym_item(param.formal.to_owned(), cgv);
        }

        self.builder.position_at_end(fn_blk);
        if let Some(retval) = self._codegen_block_0(&defn.body)? {
            self.builder
                .build_return(Some(&retval.try_get_basic_value()?));
        } else {
            self.builder.build_return(None);
        }

        if fn_xx.verify(true) {
            if self.config.optlv != OptLv::Debug {
                self.fpm.run_on(&fn_xx);
            }
        }

        self.pop_scope();

        Ok(())
    }

    fn _codegen_block_0(
        &mut self,
        scope: &Rc<RefCell<BaScope>>,
    ) -> Result<Option<CGValue<'ctx>>, Box<dyn Error>> {
        let scope_ref = scope.as_ref().borrow();

        self.codegen_stmts(&scope_ref.stmts)?;

        Ok(if let Some(ref prival) = scope_ref.tailval {
            Some(self.pri2value(prival)?)
        } else {
            None
        })
    }

    fn codegen_block(
        &mut self,
        scope: &Rc<RefCell<BaScope>>,
    ) -> Result<(), Box<dyn Error>> {
        self.push_scope();
        self._codegen_block_0(scope)?;
        self.pop_scope();

        Ok(())
    }

    fn baty_to_basicty(&self, baty: &BaType) -> Option<BasicTypeEnum<'ctx>> {
        if let BaType::VoidUnit = baty {
            return None;
        }

        Some(match &baty {
            BaType::Float => {
                BasicTypeEnum::<'ctx>::FloatType(self.context.f64_type())
            }
            BaType::I64 => {
                BasicTypeEnum::<'ctx>::IntType(self.context.i64_type())
            }
            BaType::Int => {
                BasicTypeEnum::<'ctx>::IntType(self.context.i32_type())
            }
            BaType::RawStr => BasicTypeEnum::<'ctx>::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic),
            ),
            BaType::USize => {
                if cfg!(target_pointer_width = "64") {
                    BasicTypeEnum::<'ctx>::IntType(self.context.i64_type())
                } else if cfg!(target_pointer_width = "32") {
                    BasicTypeEnum::<'ctx>::IntType(self.context.i32_type())
                } else {
                    unimplemented!()
                }
            }
            BaType::U8 => {
                BasicTypeEnum::<'ctx>::IntType(self.context.i8_type())
            }
            BaType::Customized(_typeid_rc) => {
                todo!()
            }
            BaType::ExRefFunProto(_exref_funproto) => {
                todo!()
            }
            BaType::Arr(_) => unimplemented!(),
            BaType::VoidUnit => unreachable!(),
            _ => unreachable!()
        })
    }

    fn baty_to_retty(
        &self,
        baty: &BaType,
    ) -> Either<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match &baty {
            BaType::VoidUnit => Either::Right(self.context.void_type()),
            _ => Either::Left(self.baty_to_basicty(baty).unwrap()),
        }
    }

    fn codegen_declare(
        &mut self,
        dec: &BaDeclare,
    ) -> Result<(), Box<dyn Error>> {
        let id = &dec.name;

        let res = match &dec.value {
            BaDecVal::PriVal(prival) => self.codegen_prival(prival)?,
            BaDecVal::FunCall(funcall) => {
                if let Some(cgval) = self.codegen_funcall(funcall)?.left() {
                    cgval
                } else {
                    unreachable!()
                }
            }
            BaDecVal::TwoAddr(bop, fstprival, sndprival) => {
                self.codegen_2addrexpr(bop, fstprival, sndprival)?
            }
            BaDecVal::IterBlock(iterblk) => {
                self.codegen_iter_block(iterblk)?
            }
        };

        self.insert_sym_item(id.name.to_string(), res);

        Ok(())
    }

    fn codegen_iter_block(
        &mut self,
        iterblock: &BaIterBlock,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        /* Build Outter Variable*/
        let outter_val = self.codegen_prival(&iterblock.var_outter)?;
        let cgarr: CGArr = outter_val.try_into()?;  // as array


        if cgarr.len == 0 {
            return Ok(CGValue::Pure(CGPureValue::VoidUnit))
        }

        /* Enter Name Scope */
        self.push_scope();


        /* Build Iter Control Variable(Index) */
        let fnval = self.nested_funval_stack.peek().unwrap().clone();
        let i64_t = self.context.i64_type();
        let int_arr_idx_alloca = create_entry_block_alloca(
            &self.context,
            fnval.get_last_basic_block().unwrap(),
            i64_t.into(),
            "iter-idx-alloca",
        );
        let idx_init_val = i64_t.const_int(0, false);  // idx start from 0
        self.builder.build_store(int_arr_idx_alloca, idx_init_val);
        let total_elems = i64_t
            .const_int(cgarr.len.try_into().unwrap(), false);
        let step_val = i64_t.const_int(1 as u64, false);


        /* Build Loop Block */
        let blk_loop = self.context.append_basic_block(fnval, "loop:body");
        self.builder.build_unconditional_branch(blk_loop);

        /* Update Index */
        self.builder.position_at_end(blk_loop);
        let cur_idx = self
            .builder
            .build_load(int_arr_idx_alloca, "")
            .try_into()
            .unwrap();
        let nxt_idx = self.builder.build_int_add(cur_idx, step_val, "");
        self.builder
            .build_store::<IntValue>(int_arr_idx_alloca, nxt_idx.into());


        /* Setup Formal Variable */
        let cur_elem_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(cgarr.start, &[cur_idx.into()], "")
        };
        let cur_elem = self.builder.build_load(cur_elem_ptr, "");
        let cur_elem_val = CGValue::try_from((cgarr.ty , cur_elem))?;
        self.insert_sym_item(iterblock.var_formal.to_owned(), cur_elem_val);


        /* Build Iter Loop */
        self._codegen_block_0( &iterblock.body)?;


        /* Build Exit Loop Block Test */
        let end_cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            nxt_idx,
            total_elems,
            "",
        );
        let blk_after = self.context.append_basic_block(fnval, "loop:end");
        self.builder
            .build_conditional_branch(end_cond, blk_loop, blk_after);
        self.builder.position_at_end(blk_after);


        /* Exit Name Scope */
        self.pop_scope();


        /* Collected Value */
        // without collect
        Ok(CGValue::Pure(CGPureValue::VoidUnit))
    }

    fn codegen_prival(
        &mut self,
        prival: &BaPriVal,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        match prival {
            BaPriVal::Lit(lit) => self.codegen_lit(lit),
            BaPriVal::Id(valid) => {
                if let Some(cgval) = self.get_sym_item(&valid.name) {
                    Ok(cgval)
                } else {
                    Err(TrapCode::UnresolvedSymbol(valid).emit_box_err())
                }
            }
            BaPriVal::Vector(vec) => self.codegen_vector(vec),
            BaPriVal::Range(_range) => todo!()
        }
    }

    fn codegen_vector(
        &mut self,
        vec: &BaVector,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        match &vec {
            BaVector::Array(arr) => self.codegen_array(arr),
        }
    }

    fn codegen_array(
        &mut self,
        arr: &BaArray,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        let baty = arr.ty.clone().unwrap();
        let bt = self.baty_to_basicty(&baty).unwrap();

        let elems_len = self
            .context
            .i64_type()
            .const_int(arr.elems.len().try_into().unwrap(), false);

        let elem_vals = arr
            .elems
            .iter()
            .map(|elem| {
                self.pri2value(elem).unwrap().try_get_basic_value().unwrap()
            })
            .collect_vec();

        let start_ptr =
            self.builder.build_array_malloc(bt, elems_len, "BaRawArr")?;

        array_assign(&self.context, &self.builder, elem_vals, start_ptr);

        // 0 cap: usize
        // 1 len: usize
        // 2 ptr: T*
        let arr_t = self.context.struct_type(
            &[
                self.usize_type(),
                self.usize_type(),
                bt.ptr_type(AddressSpace::Generic).into(),
            ],
            false,
        );
        let arr_ptr = self.builder.build_malloc(arr_t, "BaArray")?;
        let arr_val = self.context.const_struct(
            &[elems_len.into(), elems_len.into(), start_ptr.into()],
            false,
        );
        self.builder.build_store(arr_ptr, arr_val);

        Ok(CGValue::Ref(CGRef::Arr(Rc::new(RefCell::new(CGArr {
            ty: arr.ty.clone().unwrap(),
            len: arr.elems.len(),
            capacity: arr.elems.len(),
            start: start_ptr,
        })))))
    }

    fn codegen_funcall(
        &mut self,
        funcall: &BaFunCall,
    ) -> Result<Either<CGValue<'ctx>, CGVoid>, Box<dyn Error>> {
        // search fun ref
        let fid = &funcall.name;

        let fnval;
        let retty;

        match fid.splid {
            Some(BaSplId::RS) => {
                let funtype = if let Some(proto) = search_rs_lib(&fid.name) {
                    retty = proto.ret.as_type();

                    self.exproto_to_funtype(proto)
                } else {
                    unreachable!()
                };

                fnval = if let Some(_fnval) =
                    self.module.get_function(&fid.name)
                {
                    _fnval
                } else {
                    self.module.add_function(
                        &fid.name,
                        funtype,
                        Some(Linkage::External),
                    )
                };
            }
            Some(_) => {
                unimplemented!()
            }
            None => {
                let funkey = funcall.get_fun_key()?;

                fnval = if let Some(fn_xx) = self.funtbl.get(&funkey) {
                    retty = fn_xx.hdr.ret.clone();
                    fn_xx.val.clone()
                } else {
                    unreachable!("Unfound {:#?}", funcall)
                };
            }
        }

        // codegen fun call args
        let args_vec = self.codegen_args(&funcall.args[..])?;
        let bv_vec = args_vec
            .into_iter()
            .map(|cgval| cgval.try_get_basic_value().unwrap().into())
            .collect_vec();

        let rtn_callsite = self.builder.build_call(
            fnval,
            &bv_vec[..],
            format!("call_{}", &fid.name).as_str(),
        );

        let funrtn_either = rtn_callsite.try_as_basic_value();

        Ok(if let Some(bv) = funrtn_either.left() {
            Either::Left(CGValue::try_from((retty, bv))?)
        } else {
            Either::Right(CGVoid)
        })
    }

    fn funkey_ret_to_funtype(
        &self,
        funkey: &BaFunKey,
        ret: &BaType,
    ) -> FunctionType<'ctx> {
        let params = funkey
            .1
            .iter()
            .map(|baty| self.baty_to_basicty(baty).unwrap().into())
            .collect_vec();

        match self.baty_to_retty(ret) {
            Either::Left(ty) => ty.fn_type(&params[..], false),
            Either::Right(void_t) => void_t.fn_type(&params[..], false),
        }
    }

    fn codegen_args(
        &mut self,
        prival_iter: &[BaPriVal],
    ) -> Result<Vec<CGValue<'ctx>>, Box<dyn Error>> {
        let mut res = vec![];

        for prival in prival_iter {
            match prival {
                BaPriVal::Lit(lit) => res.push(self.codegen_lit(lit)?),
                BaPriVal::Id(id) => {
                    if let Some(ptrval) = self.get_sym_item(&id.name) {
                        res.push(ptrval.clone());
                    } else {
                        return Err(
                            TrapCode::UnresolvedSymbol(id).emit_box_err()
                        );
                    }
                }
                BaPriVal::Vector(_vec) => {
                    todo!()
                },
                BaPriVal::Range(_range) => todo!()
            }
        }

        Ok(res)
    }

    fn codegen_2addrexpr(
        &mut self,
        bop: &BaBOp,
        fstpri: &BaPriVal,
        sndpri: &BaPriVal,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        let cgval1st = self.codegen_prival(fstpri)?;
        let cgval2nd = self.codegen_prival(sndpri)?;

        let bv1st = cgval1st.try_get_basic_value()?;
        let bv2nd = cgval2nd.try_get_basic_value()?;

        let bvres: BasicValueEnum = match bop {
            &BaBOp::Add => match bv1st {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_add(
                        self.try_as_int_val(bv1st)?,
                        self.try_as_int_val(bv2nd)?,
                        "",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_add(
                        self.try_as_float_val(bv1st)?,
                        self.try_as_float_val(bv2nd)?,
                        "",
                    )
                    .into(),
                _ => {
                    unreachable!("{:?}", bv1st);
                }
            },
            &BaBOp::Sub => match bv1st {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_sub(
                        self.try_as_int_val(bv1st)?,
                        self.try_as_int_val(bv2nd)?,
                        "",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_sub(
                        self.try_as_float_val(bv1st)?,
                        self.try_as_float_val(bv2nd)?,
                        "",
                    )
                    .into(),
                _ => {
                    unreachable!("{:?}", bv1st);
                }
            },
            &BaBOp::Mul => match bv1st {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_mul(
                        self.try_as_int_val(bv1st)?,
                        self.try_as_int_val(bv2nd)?,
                        "",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_mul(
                        self.try_as_float_val(bv1st)?,
                        self.try_as_float_val(bv2nd)?,
                        "",
                    )
                    .into(),
                _ => {
                    unreachable!("{:?}", bv1st);
                }
            },
            &BaBOp::Div => match bv1st {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_signed_div(
                        self.try_as_int_val(bv1st)?,
                        self.try_as_int_val(bv2nd)?,
                        "",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_div(
                        self.try_as_float_val(bv1st)?,
                        self.try_as_float_val(bv2nd)?,
                        "",
                    )
                    .into(),
                _ => {
                    unreachable!("{:?}", bv1st);
                }
            },
            &BaBOp::Mod => match bv1st {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_signed_rem(
                        self.try_as_int_val(bv1st)?,
                        self.try_as_int_val(bv2nd)?,
                        "",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_rem(
                        self.try_as_float_val(bv1st)?,
                        self.try_as_float_val(bv2nd)?,
                        "",
                    )
                    .into(),
                _ => {
                    unreachable!("{:?}", bv1st);
                }
            },
        };

        let cgpureval: CGPureValue = cgval1st.try_into()?;

        Ok(CGValue::Pure(cgpureval.fork_with(bvres)))
    }

    fn usize_type(&self) -> BasicTypeEnum<'ctx> {
        if usize_len() == 8 {
            self.context.i64_type().into()
        } else {
            self.context.i32_type().into()
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Basic Type Cast

    fn try_as_float_val(
        &self,
        vale: BasicValueEnum<'ctx>,
    ) -> Result<FloatValue<'ctx>, Box<dyn Error>> {
        match vale {
            BasicValueEnum::FloatValue(v) => Ok(v),
            _ => {
                unreachable!()
            }
        }
    }

    fn try_as_int_val(
        &self,
        vale: BasicValueEnum<'ctx>,
    ) -> Result<IntValue<'ctx>, Box<dyn Error>> {
        match vale {
            BasicValueEnum::IntValue(v) => Ok(v),
            _ => {
                unreachable!("try as int {:?}", vale)
            }
        }
    }

    fn codegen_lit(&self, lit: &BaLit) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        match lit {
            BaLit::I32(i32val) => {
                let i32_t = self.context.i32_type();

                CGValue::try_from((
                    lit.to_batype(),
                    i32_t.const_int(i32val.val as u64, false).into(),
                ))
            }
            BaLit::Str(bastr) => {
                let strval = &bastr.val;
                let i8_t = self.context.i8_type();
                let i64_t = self.context.i64_type();

                let mut cchars = strval
                    .as_bytes()
                    .into_iter()
                    .map(|x| i8_t.const_int(*x as u64, false))
                    .collect_vec();

                cchars.push(i8_t.const_zero());

                let cchars_len = i64_t.const_int(
                    (strval.len() + 1 as usize).try_into().unwrap(),
                    false,
                );

                // 栈上分配
                let cchars_ptr = self.builder.build_array_alloca(
                    i8_t,
                    cchars_len,
                    &bastr.tag_str(),
                );
                // // 堆上分配
                // let cchars_ptr
                // = self.builder.build_array_malloc(i8_strarr_t, cchars_len, "buildstr")?;
                for (i, cchar_int) in cchars.into_iter().enumerate() {
                    let idx = i64_t.const_int(i as u64, false);
                    let ptr = unsafe {
                        self.builder.build_gep(cchars_ptr, &[idx.into()], "")
                    };
                    self.builder.build_store(ptr, cchar_int);
                }

                CGValue::try_from((lit.to_batype(), cchars_ptr.into()))
            }
            _ => unreachable!(),
        }
    }

    fn pri2value(
        &mut self,
        prival: &BaPriVal,
    ) -> Result<CGValue<'ctx>, Box<dyn Error>> {
        match &prival {
            BaPriVal::Lit(lit) => self.codegen_lit(&lit),
            BaPriVal::Id(id) => Ok(self.get_sym_item(&id.name).unwrap()),
            BaPriVal::Vector(vec) => self.codegen_vector(vec),
            BaPriVal::Range(_range) => todo!()
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Foreign Type Cast

    fn exproto_to_funtype(&self, proto: &CFun) -> FunctionType<'ctx> {
        let params = proto
            .params
            .iter()
            .map(|param| self.exty_to_basicty(param).unwrap().into())
            .collect_vec();

        match self.exty_to_retty(&proto.ret) {
            Either::Left(ty) => ty.fn_type(&params[..], false),
            Either::Right(void_t) => void_t.fn_type(&params[..], false),
        }
    }

    fn exty_to_retty(
        &self,
        ty: &CTy,
    ) -> Either<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match &ty {
            CTy::F64 => Either::Left(BasicTypeEnum::<'ctx>::FloatType(
                self.context.f64_type(),
            )),
            CTy::I64 => Either::Left(BasicTypeEnum::<'ctx>::IntType(
                self.context.i64_type(),
            )),
            CTy::I32 => Either::Left(BasicTypeEnum::<'ctx>::IntType(
                self.context.i32_type(),
            )),
            CTy::Void => Either::Right(self.context.void_type()),
            CTy::CStr => Either::Left(BasicTypeEnum::<'ctx>::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic),
            )),
        }
    }

    fn exty_to_basicty(&self, ty: &CTy) -> Option<BasicTypeEnum<'ctx>> {
        match self.exty_to_retty(ty) {
            Either::Left(basic) => Some(basic),
            Either::Right(_) => None,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// Helper Function

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

/// Assign Array Value
pub fn array_assign<'ctx>(
    context: &'ctx Context,
    builder: &'ctx Builder<'ctx>,
    elems: Vec<BasicValueEnum<'ctx>>,
    start_ptr: PointerValue<'ctx>,
) {
    let i64_t = context.i64_type();

    for (i, elem) in elems.into_iter().enumerate() {
        let idx = i64_t.const_int(i as u64, false);
        let ptr = unsafe { builder.build_gep(start_ptr, &[idx.into()], "") };
        builder.build_store(ptr, elem);
    }
}

/// == usize_type
pub fn isize_type<'ctx>(context: &'ctx Context) -> IntType<'ctx> {
    match usize_len() {
        8 => context.i64_type(),
        4 => context.i32_type(),
        _ => unimplemented!()
    }
}


#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use bacommon::config::{CompilerConfig, EmitType, OptLv, PrintTy, TargetType};
    use bacommon::lexer::SrcFileInfo;

    use crate::frontend::lexer::{tokenize, trim_tokens};
    use crate::frontend::manual_parser::Parser;
    use crate::{VerboseLv, VERBOSE};

    use crate::middleware::ml_simplifier::MLSimplifier;

    use super::codegen;

    #[test]
    fn test_algb_op() {
        println!("{}", 1f64 + (3.0 / 2.0) - 2f64);

        println!("-4: {}", (0 - 4) as u64);
        println!("-4: {}", (0 - 4) as u64 as i32);
    }

    #[test]
    #[allow(unused)]
    fn test_codegen() {
        let file =
            SrcFileInfo::new(PathBuf::from("./examples/exp2.ba")).unwrap();
        let out = PathBuf::from("out.o");
        let config = CompilerConfig {
            optlv: OptLv::Debug,
            target_type: TargetType::Bin,
            emit_type: EmitType::LLVMIR,
            // emit_type: crate::EmitType::Obj,
            // print_type: PrintTy::File(out),
            print_type: PrintTy::StdErr,
        };

        let tokens = tokenize(&file);
        let tokens = trim_tokens(tokens);

        let mut parser = Parser::new(tokens);
        let ml = parser.parse().unwrap();

        let mlslf = MLSimplifier::new(ml);
        let bin = mlslf.simplify().unwrap();

        codegen(&config, file, bin).unwrap();
    }
}
