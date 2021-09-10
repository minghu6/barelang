#![allow(unused_imports)]

use inkwell::OptimizationLevel;
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
    AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, VoidType,
    PointerType
};
use inkwell::values::{BasicValue, BasicValueEnum, CallSiteValue, FloatValue, FunctionValue, InstructionValue, IntValue, PointerValue};
use inkwell::AddressSpace;

use inkwell::debug_info::{
    DebugInfoBuilder, DWARFSourceLanguage, DWARFEmissionKind
};


use itertools::{Either, Itertools};
use lazy_static::lazy_static;

use indexmap::{IndexMap, indexset};

use std::convert::TryInto;
use std::error::Error;
use std::path::Path;
use std::env;

use crate::datalsp::*;
use crate::datair::*;
use crate::*;
use crate::dbi::DebugInfo;
use crate::error::{BaCErr, TrapCode};
use crate::ml_simplifier::gensym_rand;
use crate::rsclib::search_rs_lib;


pub fn codegen(config: &CompilerConfig, bin: &BaBin) -> Result<(), Box<dyn Error>> {
    let context = Context::create();

    let mut codegen
    = CodeGen::new(
        &context,
        config
    );

    codegen.codegen(bin)
}


#[allow(unused)]
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,
    namedvars: IndexMap<String, PointerValue<'ctx>>,
    config: &'ctx CompilerConfig,
    dbi: DebugInfo<'ctx>
}


#[allow(dead_code)]
impl<'ctx> CodeGen<'ctx> {
    ///////////////////////////////////////////////////////////////////////////
    //// CodeGen Init

    fn new(context: &'ctx Context, config: &'ctx CompilerConfig) -> Self {
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

        Self {
            module,
            builder: context.create_builder(),
            context: &context,
            fpm,
            namedvars: IndexMap::new(),
            config,
            dbi
        }
    }

    pub fn codegen(&mut self, bin: &BaBin) -> Result<(), Box<dyn Error>> {
        match self.config.target_type {
            TargetType::Bin => {
                self.codegen_bin(bin)
            },
            _ => unimplemented!()
        }
    }

    pub fn codegen_bin(&mut self, bin: &BaBin) -> Result<(), Box<dyn Error>> {
        self.begin_main();

        for stmt in bin.stmts.iter() {
            match stmt {
                BaStmt::Declare(dec) => {
                    self.codegen_declare(dec)?;
                },
                BaStmt::FunCall(funcall) => {
                    self.codegen_funcall(funcall)?;
                }
            }
        }

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
            &self.config.objpath
        )?;

        Ok(())
    }

    fn emit_llvmir(&self) -> Result<(), Box<dyn Error>> {
        self.module
        .print_to_file(&self.config.objpath)
        .map_err(|llvmstr| BaCErr::new_box_err(llvmstr.to_str().unwrap()))
    }

    ///////////////////////////////////////////////////////////////////////////
    //// Entry point codegen

    fn begin_main(&self) {
        let i64_type = self.context.i64_type();
        let fn_main_t = i64_type.fn_type(&[], false);
        let fn_main = self.module.add_function("main", fn_main_t, None);
        let blk_main = self.context.append_basic_block(fn_main, "mainblk");
        self.builder.position_at_end(blk_main);
    }

    fn end_main(&self, rtn: IntValue<'ctx>) {
        self.builder.build_return(Some(&rtn));
        let main_fn = self.module.get_function("main").unwrap();

        if self.config.optlv != OptLv::Debug {
            self.fpm.run_on(&main_fn);
        }
    }

    fn end_main_ok(&self) {
        let i64_type = self.context.i64_type();

        self.end_main(i64_type.const_zero())
    }

    ///////////////////////////////////////////////////////////////////////////
    //// BaStmt codegen

    fn codegen_declare(&mut self, dec: &BaDeclare) -> Result<(), Box<dyn Error>> {
        let id = &dec.name;
        let ptr;

        match &dec.value {
            BaDecVal::PriVal(prival) => {
                ptr = self.codegen_prival(prival)?;
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
                    let basicval = funrtn_either.left().unwrap();
                    if let Some(ret_t)
                    = funrtn_callsite.get_called_fn_value().get_type().get_return_type() {
                        ptr = self.builder.build_alloca(ret_t, id.name.as_str());
                        self.builder.build_store(ptr, basicval);
                    }
                    else {
                        return Err(
                            BaCErr::new_box_err(
                                format!("fun proto {:?} return None", funcall.name).as_str()
                            )
                        )
                    }
                }
            },
            BaDecVal::TwoAddr(bop, fstprival, sndprival) => {
                ptr = self.codegen_2addrexpr(bop, fstprival, sndprival)?;
            },
        }

        self.namedvars.insert(id.name.to_string(), ptr);

        Ok(())
    }

    fn codegen_prival(&mut self, prival: &BaPriVal)
    -> Result<PointerValue<'ctx>, Box<dyn Error>>
    {
        let ptr;

        match prival {
            BaPriVal::Lit(lit) => {
                match lit {
                    BaLit::I32(i32val) => {
                        let i32_t = self.context.i32_type();
                        let value = i32_t.const_int(i32val.val as u64, false);
                        ptr = self.builder.build_alloca(i32_t, &gensym_rand());
                        self.builder.build_store(ptr, value);

                    },
                    BaLit::Str(bastr) => {
                        let strval = &bastr.val;
                        let i8_t = self.context.i8_type();
                        let i64_t = self.context.i64_type();
                        let i8ptr_t = i8_t.ptr_type(AddressSpace::Generic);

                        let cchars
                        = strval
                        .as_bytes()
                        .into_iter()
                        .map(|x| i8_t.const_int(*x as u64, false))
                        .collect_vec();

                        let cchars_len
                        = i64_t.const_int(
                            (strval.len() as usize).try_into().unwrap(),
                            false
                        );

                        let cchars_ptr
                        = self.builder.build_array_alloca(i8ptr_t, cchars_len, &gensym_rand());
                        let cchars_val = i8_t.const_array(&cchars[..]);

                        self.builder.build_store(cchars_ptr, cchars_val);

                        ptr = self.builder.build_alloca(
                            i8ptr_t.ptr_type(AddressSpace::Generic),
                            &gensym_rand()
                        );
                        self.builder.build_store(ptr, cchars_ptr);
                    },
                    _ => unreachable!()
                }
            },
            BaPriVal::Id(valid) => {
                if let Some(valsymptr) = self.namedvars.get(&valid.name) {
                    ptr = valsymptr.clone();
                }
                else {
                    return Err(
                        TrapCode::UnresolvedSymbol(valid).emit_box_err()
                    )
                }
            },
        }

        Ok(ptr)
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
                    funtype = self.exproto2funtype(proto);
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
                unreachable!("None splid {:?}", fid)
            }
        }

        // codegen fun call args
        let args_vec = self.codegen_args(&funcall.args[..])?;

        let rtn = self.builder.build_call(
            fnval,
            &args_vec[..],
            &fid.name
        );

        Ok(rtn)
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
                    if let Some(ptrval) = self.namedvars.get(&id.name) {
                        res.push(
                            self.builder.build_load(*ptrval, "")
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
    -> Result<PointerValue<'ctx>, Box<dyn Error>>
    {
        let fstptr = self.codegen_prival(fstpri)?;
        let sndptr = self.codegen_prival(sndpri)?;
        let fst_bv
        = self.builder.build_load(fstptr, "");
        let snd_bv
        = self.builder.build_load(sndptr, "");

        let ptr;

        match bop {
            &BaBOp::Add => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        let res = self.builder.build_int_add(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), &gensym_rand());
                        self.builder.build_store(ptr, res);
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let res = self.builder.build_float_add(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), &gensym_rand());
                        self.builder.build_store(ptr, res);
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
            &BaBOp::Sub => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        let res = self.builder.build_int_sub(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), &gensym_rand());
                        self.builder.build_store(ptr, res);
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let res = self.builder.build_float_sub(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), &gensym_rand());
                        self.builder.build_store(ptr, res);
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
            &BaBOp::Mul => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        let res = self.builder.build_int_mul(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let res = self.builder.build_float_mul(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            }
            &BaBOp::Div => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        let res = self.builder.build_int_signed_div(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let res = self.builder.build_float_div(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }

            },
            &BaBOp::Mod => {
                match fst_bv {
                    BasicValueEnum::IntValue(_) => {
                        let res = self.builder.build_int_signed_rem(
                            self.try_as_int_val(fst_bv)?,
                            self.try_as_int_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    BasicValueEnum::FloatValue(_) => {
                        let res = self.builder.build_float_rem(
                            self.try_as_float_val(fst_bv)?,
                            self.try_as_float_val(snd_bv)?,
                            ""
                        );
                        ptr = self.builder.build_alloca(res.get_type(), "");
                        self.builder.build_store(ptr, res);
                    },
                    _ => {
                        unreachable!("{:?}", fst_bv);
                    }
                }
            },
        }

        Ok(ptr)
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
        match lit {
            BaLit::I32(i32val) => {
                let i32_t = self.context.i32_type();
                let value = i32_t.const_int(i32val.val as u64, false);

                BasicValueEnum::IntValue(value)
            },
            BaLit::Float(f64val) => {
                let f64_t = self.context.f64_type();
                let value = f64_t.const_float(f64val.val);

                BasicValueEnum::FloatValue(value)
            }
            _ => unreachable!()
        }
    }

    fn exproto2funtype(&self, proto: &ExRefFunProto) -> FunctionType<'ctx> {
        let params = proto.params.iter().map(|param| {
            self.exrefty2basicty(param).unwrap()
        }).collect_vec();

        match self.exref2retty(&proto.ret) {
            Either::Left(tye) => {
                tye.fn_type(&params[..], false)
            },
            Either::Right(void_t) => {
                void_t.fn_type(&params[..], false)
            }
        }
    }

    fn exref2retty(&self, ty: &ExRefType) -> Either<BasicTypeEnum<'ctx>, VoidType<'ctx>> {
        match &ty {
            ExRefType::F64  => Either::Left(BasicTypeEnum::<'ctx>::FloatType(
                self.context.f64_type()
            )),
            ExRefType::I64  => Either::Left(BasicTypeEnum::<'ctx>::IntType(
                self.context.i64_type()
            )),
            ExRefType::Void => Either::Right(self.context.void_type()),
            ExRefType::Str => Either::Left(BasicTypeEnum::<'ctx>::PointerType(
                self.context.i8_type().ptr_type(AddressSpace::Generic)
            ))
        }
    }

    fn exrefty2basicty(&self, ty: &ExRefType) -> Option<BasicTypeEnum<'ctx>> {
        match &ty {
            ExRefType::F64  => Some(BasicTypeEnum::<'ctx>::FloatType(
                self.context.f64_type()
            )),
            ExRefType::I64  => Some(BasicTypeEnum::<'ctx>::IntType(
                self.context.i64_type()
            )),
            ExRefType::Void => {
                None
            },
            ExRefType::Str => {
                Some(BasicTypeEnum::<'ctx>::PointerType(
                    self.context.i8_type().ptr_type(AddressSpace::Const)
                ))
            }
        }
    }

    // #[inline]
    // fn i64_t(&self) -> IntType {
    //     self.context.i64_type()
    // }

    // #[inline]
    // fn i8_t(&self) -> IntType {
    //     self.context.i8_type()
    // }

    // #[inline]
    // fn i8ptr_t(&self) -> PointerType {
    //     self.i8_t().ptr_type(AddressSpace::Generic)
    // }

    // #[inline]
    // fn i8ptr_const_t(&self) -> PointerType {
    //     self.i8_t().ptr_type(AddressSpace::Const)
    // }
}




impl<'ctx> CodeGen<'ctx> {

}


////////////////////////////////////////////////////////////////////////////////
//// Helper Function







#[cfg(test)]
mod test {
    #[test]
    fn test_algb_op() {
        println!("{}", 1f64+(3.0/2.0) -2f64);

        println!("-4: {}", (0-4) as u64);
        println!("-4: {}", (0-4) as u64 as i32);

    }


}
