use std::{
    collections::HashSet,
    error::Error,
    marker::PhantomData,
    path::{Path, PathBuf},
    rc::Rc,
};

use bacommon::{
    config::{CompilerConfig, PrintTy},
    target_generator::TargetGenerator,
    vmbuilder::builder_position_at_start,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue},
    AddressSpace,
};
use itertools::Itertools;
use lisparser::{
    data::*,
    parser::{LispParser, LispParserConfig},
    *,
};
use m6stack::AnIteratorWrapper;

use crate::{
    core_syntax::{
        name_mangling::{concat_overload_name, NameConcatStyle},
        type_anno::*,
    },
    parse_phase_2::{self, parse_concrete_type_anno_map, parse_generic_name},
};

use super::{
    a_fn::AFn,
    a_struct::AStruct,
    a_value::AValue,
    hardcode_fns::init_fn_definition,
    name_mangling::rename_fn_alias,
    r#const::*,
    spec_etc::{extract_index_path_from_dir, name_to_path},
    CompileContext, LocalContext, LocalContextType,
};

use bacommon::error::*;

////////////////////////////////////////////////////////////////////////////////
//// `Callsite` Form

pub struct FormValue<'ctx> {
    builder: Builder<'ctx>,
    avalue_opt: Option<AValue<'ctx>>,
}

impl<'ctx> FormValue<'ctx> {
    fn new(builder: Builder<'ctx>, avalue_opt: Option<AValue<'ctx>>) -> Self {
        Self {
            builder,
            avalue_opt,
        }
    }
}

impl<'ctx> Into<Option<AValue<'ctx>>> for FormValue<'ctx> {
    fn into(self) -> Option<AValue<'ctx>> {
        self.avalue_opt
    }
}

impl<'ctx> TryInto<AValue<'ctx>> for FormValue<'ctx> {
    type Error = Box<dyn Error>;

    fn try_into(self) -> Result<AValue<'ctx>, Self::Error> {
        self.avalue_opt.ok_or(VoidAsArgError::new_box_err(""))
    }
}

////////////////////////////////////////////////////////////////////////////////
//// `NS` Form

pub struct ANS<'ctx> {
    pub name: String,             // namespace name
    pub(crate) mods: Vec<String>, // modules name belongs to the namespace
    pub(crate) path: PathBuf,
    pub(crate) ctx: CompileContext<'ctx>,
}

impl<'ctx> ANS<'ctx> {
    pub fn init(
        dir: &Path,
        vmctx: &'ctx Context,
    ) -> Result<Self, Box<dyn Error>> {
        let dirname = dir.file_name().unwrap().to_str().unwrap();
        let ctx = CompileContext::new(dirname, vmctx);
        let index_path = extract_index_path_from_dir(dir);

        let mut parser =
            LispParser::new(&index_path, LispParserConfig::default())?;
        let lm = parser.parse()?;

        let list1st: NonNilListData = lm.lists[0].clone().try_into()?;

        let head_sym: SymData = list1st.head.try_into()?;

        Ok(match head_sym.val.as_str() {
            "ns" => {
                let tail: NonNilListData = list1st.tail.try_into()?;

                let mut tail_iter = tail.flatten().into_iter();

                let ns_name_sym: SymData = tail_iter
                    .next()
                    .ok_or(XXXError::new_box_err(""))?
                    .try_into()?;
                let name = ns_name_sym.val;

                let sub_any =
                    tail_iter.next().ok_or(XXXError::new_box_err(""))?;

                let sub_tuple: BracketTupleData = sub_any.try_into()?;
                let mut mods = vec![];

                for item_any in sub_tuple.items.iter() {
                    mods.push(TryInto::<SymData>::try_into(item_any)?.val)
                }

                ANS {
                    name,
                    mods,
                    path: dir.to_owned(),
                    ctx,
                }
            }
            other => return Err(XXXError::new_box_err(other)),
        })
    }

    pub(crate) fn sub_paths(&self) -> AnIteratorWrapper<PathBuf> {
        let iter = box self.mods.iter().map(|sub| {
            let sub_name = name_to_path(sub);
            self.path.join(sub_name)
        });

        AnIteratorWrapper { iter }
    }

    pub(crate) fn load(&mut self) -> Result<(), Box<dyn Error>> {
        for sub in self.sub_paths().collect_vec().iter() {
            load_file(&sub, &mut self.ctx)?;
        }

        self.compile_ns()?;

        Ok(())
    }

    pub fn print(
        &self,
        config: &CompilerConfig,
    ) -> Result<(), Box<dyn Error>> {
        let gen = TargetGenerator::new(&self.ctx.vmmod, config);

        gen.print()
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// `Compile`

    fn compile_ns(&mut self) -> Result<(), Box<dyn Error>> {
        // Handle Function
        let fns = self.ctx.form_fn_map.values().cloned().collect_vec();

        for afn in fns.iter() {
            self.compile_declare(&afn, None)?;
        }

        for afn in fns.iter() {
            self.compile_definition(&afn)?;
        }

        // Handle Structure
        let structs = self.ctx.form_struct_map.values().cloned().collect_vec();

        for astruct in structs.iter() {
            astruct.do_circular_dependency_check(
                &self.ctx,
                &mut HashSet::new(),
            )?;
        }

        // Handle Entry
        if let Some(main_body) = self.ctx.entry.clone() {
            self.compile_entry(&main_body)?;
        }

        Ok(())
    }

    pub fn compile_struct(
        &mut self,
        astruct: &AStruct,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        if let Some(astruct_t) = self.ctx.struct_map.get(&astruct.name) {
            return Ok(astruct_t.clone().into());
        }

        let field_types = self.compile_type_anno_batch(
            &astruct
                .fields
                .iter()
                .map(|field| field.type_anno.clone())
                .collect_vec()[..],
        )?;

        let struct_t = self.ctx.vmctx.struct_type(&field_types[..], false);

        self.ctx
            .struct_map
            .insert(self.name.to_owned(), struct_t.clone());

        Ok(struct_t.into())
    }

    pub fn specific_avalue(
        &mut self,
        avalue: &AValue<'ctx>,
    ) -> Result<Box<dyn BasicValue<'ctx> + 'ctx>, Box<dyn Error>> {
        Ok(match &avalue.type_anno {
            ConcreteTypeAnno::Primitive(ref name, addr_mode) => {
                if is_int(&name) {
                    if let AddrMode::Ptr = addr_mode {
                        box avalue.vm_val.into_pointer_value()
                            as Box<dyn BasicValue<'ctx>>
                    } else {
                        box avalue.vm_val.into_int_value()
                            as Box<dyn BasicValue<'ctx>>
                    }
                } else if is_float(&name) {
                    if let AddrMode::Ptr = addr_mode {
                        box avalue.vm_val.into_pointer_value()
                            as Box<dyn BasicValue<'ctx>>
                    } else {
                        box avalue.vm_val.into_float_value()
                            as Box<dyn BasicValue<'ctx>>
                    }
                } else {
                    unreachable!()
                }
            }
            ConcreteTypeAnno::Struct(name, _addr_mode) => {
                if let Some(_vm_t) = self.ctx.struct_map.get(name) {
                } else {
                    if let Some(form_struct) =
                        self.ctx.form_struct_map.get(name.as_str()).cloned()
                    {
                        self.compile_struct(&form_struct)?;
                    } else {
                        return Err(UnknownDefineError::new_box_err(name));
                    }
                }

                box avalue.vm_val.into_pointer_value()
                    as Box<dyn BasicValue<'ctx>>
            }
        })
    }

    pub fn compile_type_anno(
        &mut self,
        type_anno: &ConcreteTypeAnno,
    ) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        Ok(match &type_anno {
            ConcreteTypeAnno::Primitive(ref name, addr_mode) => {
                match name.as_str() {
                    USIZE | I64 | U64 => {
                        let val_t = self.ctx.vmctx.i64_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    I32 | U32 => {
                        let val_t = self.ctx.vmctx.i32_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    I8 | U8 => {
                        let val_t = self.ctx.vmctx.i8_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    FLOAT | F64 => {
                        let val_t = self.ctx.vmctx.f64_type();

                        if let AddrMode::Ptr = addr_mode {
                            val_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            val_t.into()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ConcreteTypeAnno::Struct(name, addr_mode) => {
                if let Some(vm_t) = self.ctx.struct_map.get(name) {
                    if let AddrMode::Ptr = addr_mode {
                        vm_t.ptr_type(AddressSpace::Generic).into()
                    } else {
                        vm_t.clone().into()
                    }
                } else {
                    // collect all form-struct before compile
                    if let Some(form_struct) =
                        self.ctx.form_struct_map.get(name.as_str()).cloned()
                    {
                        // circular dependency check before that
                        let struct_t = self
                            .compile_struct(&form_struct)?
                            .into_struct_type();

                        if let AddrMode::Ptr = addr_mode {
                            struct_t.ptr_type(AddressSpace::Generic).into()
                        } else {
                            struct_t.into()
                        }
                    } else {
                        return Err(UnknownDefineError::new_box_err(name));
                    }
                }
            }
        })
    }

    pub(crate) fn compile_type_anno_batch(
        &mut self,
        types_anno: &[ConcreteTypeAnno],
    ) -> Result<Vec<BasicTypeEnum<'ctx>>, Box<dyn Error>> {
        let mut vm_types = Vec::with_capacity(types_anno.len());

        for type_anno in types_anno.iter() {
            vm_types.push(self.compile_type_anno(type_anno)?)
        }

        Ok(vm_types)
    }

    /// Compile Function Declare
    pub(crate) fn compile_declare(
        &mut self,
        afn: &AFn,
        linkage: Option<Linkage>,
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        if let Some(fn_val) = self.ctx.vmmod.get_function(&afn.vm_name()) {
            return Ok(fn_val);
        }

        let mut param_types = Vec::with_capacity(afn.params.len());

        for param in afn.params.iter() {
            param_types.push(self.compile_type_anno(&param.type_anno)?.into())
        }

        let fn_t = match &afn.ret {
            Some(type_anno) => {
                let ret_t = self.compile_type_anno(type_anno)?;

                ret_t.fn_type(&param_types[..], false)
            }
            None => {
                let ret_t = self.ctx.vmctx.void_type();

                ret_t.fn_type(&param_types[..], false)
            }
        };

        let fn_val =
            self.ctx
                .vmmod
                .add_function(&afn.vm_name(), fn_t.clone(), linkage);

        self.ctx.fn_map.insert(afn.vm_name(), fn_t.clone());

        Ok(fn_val)
    }

    /// Compile Function Definition
    pub(crate) fn compile_entry(
        &mut self,
        body: &ListData,
    ) -> Result<(), Box<dyn Error>> {
        let i32_t = self.ctx.vmctx.i32_type();
        let const_char_ptr_t =
            self.ctx.vmctx.i8_type().ptr_type(AddressSpace::Const);
        let cosnt_const_char_ptr_ptr_t =
            const_char_ptr_t.ptr_type(AddressSpace::Const);

        let fn_main_t = i32_t.fn_type(
            &[
                i32_t.into(),                      // argc
                cosnt_const_char_ptr_ptr_t.into(), // argv
                cosnt_const_char_ptr_ptr_t.into(), // envp
            ],
            false,
        );
        let fn_main = self.ctx.vmmod.add_function("main", fn_main_t, None);

        let builder = init_fn_definition(self, fn_main);

        match body {
            ListData::Nil(_) => {
                builder.build_return(Some(&i32_t.const_zero()));
            }
            ListData::NonNil(nonnillist) => {
                // TODO: handle argc/argv/envp
                let lctx = self.ctx.fn_top_lctx(&[], fn_main);
                let ret_form = self.eval_form(
                    lctx,
                    nonnillist.clone(),
                )?;

                match ret_form.avalue_opt {
                    Some(avalue) => {
                        builder.build_return(Some(&avalue.vm_val))
                    },
                    None => builder.build_return(Some(&i32_t.const_zero())),
                };
            }
        };

        Ok(())
    }

    /// Compile Function Definition
    pub(crate) fn compile_definition(
        &mut self,
        afn: &AFn,
    ) -> Result<Option<AValue<'ctx>>, Box<dyn Error>> {
        let fn_val = self.ctx.vmmod.get_function(&afn.vm_name()).unwrap();
        let lctx = self.ctx.fn_top_lctx(&afn.params[..], fn_val);
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let ret = match &afn.body {
            ListData::Nil(_) => {
                if let Some(ref type_anno) = afn.ret {
                    return Err(MissMatchedRetTyError::new_box_err(
                        &type_anno.to_string(),
                    ));
                } else {
                    builder.build_return(None);
                    None
                }
            }
            ListData::NonNil(nonnillist) => {
                self.eval_form(lctx, nonnillist.clone())?.into()
            }
        };

        self.ctx.current_fn = Some(fn_val);

        Ok(ret)
    }

    fn compile_lsp_lit(&self, lsplit: LitData) -> AValue<'ctx> {
        let type_anno;
        let vm_val;
        match lsplit {
            LitData::Int(int) => match (int.len, int.signed) {
                (1, true) => {
                    type_anno = ConcreteTypeAnno::i8();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i8_type()
                        .const_int(int.val as u64, true)
                        .into()
                }
                (1, false) => {
                    type_anno = ConcreteTypeAnno::u8();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i8_type()
                        .const_int(int.val as u64, false)
                        .into()
                }
                (2, true) => {
                    type_anno = ConcreteTypeAnno::i16();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i16_type()
                        .const_int(int.val as u64, true)
                        .into()
                }
                (2, false) => {
                    type_anno = ConcreteTypeAnno::u16();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i16_type()
                        .const_int(int.val as u64, false)
                        .into()
                }
                (4, true) => {
                    type_anno = ConcreteTypeAnno::i32();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i32_type()
                        .const_int(int.val as u64, true)
                        .into()
                }
                (4, false) => {
                    type_anno = ConcreteTypeAnno::u32();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i32_type()
                        .const_int(int.val as u64, false)
                        .into()
                }
                (8, true) => {
                    type_anno = ConcreteTypeAnno::i64();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i64_type()
                        .const_int(int.val as u64, true)
                        .into()
                }
                (8, false) => {
                    type_anno = ConcreteTypeAnno::u64();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i64_type()
                        .const_int(int.val as u64, false)
                        .into()
                }
                (16, true) => {
                    type_anno = ConcreteTypeAnno::i128();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i128_type()
                        .const_int(int.val as u64, true)
                        .into()
                }
                (16, false) => {
                    type_anno = ConcreteTypeAnno::u128();
                    vm_val = self
                        .ctx
                        .vmctx
                        .i128_type()
                        .const_int(int.val as u64, false)
                        .into()
                }

                _ => unimplemented!(),
            },
            LitData::Float(float) => {
                type_anno = ConcreteTypeAnno::f64();
                vm_val =
                    self.ctx.vmctx.f64_type().const_float(float.val).into();
            }
            LitData::Str(str) => {
                vm_val = self
                    .ctx
                    .vmctx
                    .const_string(str.val.as_bytes(), true)
                    .into();

                type_anno = ConcreteTypeAnno::Primitive(
                    CONST_STR.to_owned(),
                    AddrMode::Value,
                );
            }
        }

        AValue { type_anno, vm_val }
    }

    fn type_anno_try_into_form_struct(
        &self,
        type_anno: &ConcreteTypeAnno,
    ) -> Result<AStruct, Box<dyn Error>> {
        if let ConcreteTypeAnno::Struct(name, addr_mode) = type_anno {
            if let AddrMode::Value = addr_mode {
                if let Some(astruct) = self.ctx.form_struct_map.get(name) {
                    return Ok(astruct.clone());
                }
            }
        }

        Err(XXXError::new_box_err(type_anno.to_string().as_str()))
    }

    ////////////////////////////////////////////////////////////////////////////////
    //// Compile Form

    pub(crate) fn eval_any(
        &mut self,
        lctx: LocalContext<'ctx>,
        val_any: Box<AnyData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let form_val = match *val_any {
            AnyData::Pri(pri) => match pri {
                PriData::Lit(lit) => {
                    FormValue::new(builder, Some(self.compile_lsp_lit(lit)))
                }
                PriData::Sym(sym) => FormValue::new(
                    builder,
                    self.ctx.get_avalue(&lctx, &sym.val).cloned(),
                ),
                _ => unimplemented!(),
            },
            AnyData::Agg(agg) => match agg {
                AggData::List(list) => {
                    self.eval_form(lctx, list.try_into()?)?
                }
                _ => unimplemented!(),
            },
        };

        Ok(form_val)
    }

    pub(crate) fn eval_form(
        &mut self,
        lctx: LocalContext<'ctx>,
        nonnillist: NonNilListData,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let head_sym: SymData = nonnillist.head.try_into()?;

        let form_name = rename_fn_alias(&head_sym.val);

        let dyn_compile = match form_name.as_str() {
            FORM_ATTR => Self::eval_form_attr,
            FORM_DEREF => Self::eval_form_deref,
            FORM_DEREF_ATTR => Self::eval_form_deref_attr,
            FORM_LET => Self::eval_form_let,
            FORM_PROGN => Self::eval_form_progn,
            FORM_IF => Self::eval_form_if,

            FORM_ADD => Self::eval_form_add,
            FORM_CREATE_STRUCT => Self::eval_form_create_struct,
            FORM_MALLOC => Self::eval_form_malloc,
            FORM_REALLOC => Self::eval_form_realloc,

            FORM_NOT => Self::eval_form_not,
            FORM_MUL => Self::eval_form_mul,
            FORM_GT => Self::eval_form_gt,

            FORM_UPATE_ATTR => Self::eval_form_update_attr,
            FORM_UPATE_INDEX => Self::eval_form_update_index,

            _ => Self::eval_norm_form,
        };

        let ret =
            dyn_compile(self, lctx, &form_name, nonnillist.tail.clone())?;

        Ok(ret)
    }

    // /// (add x y)
    // pub(crate) fn eval_form_add(
    //     &mut self,
    //     lctx: LocalContext<'ctx>,
    //     _name: &str,
    //     tail: Box<ListData>,
    // ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
    //     let tail_vec = tail.flatten();

    //     if tail_vec.len() != 2 {
    //         return Err(
    //             MalformedSyntaxError::new_box_err(
    //                 &format!("add expect 2 clause, found {}", tail_vec.len())
    //             )
    //         )
    //     }

    //     let elem1st = self.eval_any(lctx.clone(), tail_vec[0].clone().try_into())?.try_into()?;

    // }

    /// (attr sym sym)
    pub(crate) fn eval_form_attr(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        // let lctx = self.ctx.get_lctx();
        let builder = lctx.get_builder(&self.ctx.vmctx);
        let rems = tail.flatten();

        let var_name_sym: SymData = rems[0].try_into()?;
        let var_name = &var_name_sym.val;

        let field_name_sym: SymData = rems[1].try_into()?;
        let field_name = &field_name_sym.val;

        if let Some(avalue) = self.ctx.get_avalue(&lctx, var_name) {
            let astruct =
                self.type_anno_try_into_form_struct(&avalue.type_anno)?;

            if let Some(field_index) = astruct.index_of_field(field_name) {
                let vm_val = builder
                    .build_extract_value(
                        avalue.vm_val.into_struct_value(),
                        field_index as u32,
                        "",
                    )
                    .unwrap();

                let type_anno =
                    astruct.fields[field_index as usize].type_anno.clone();

                Ok(FormValue::new(builder, Some(AValue { type_anno, vm_val })))
            } else {
                Err(UnknownFieldError::new_box_err(&format!(
                    "{} of {}",
                    astruct.name, field_name
                )))
            }
        } else {
            Err(UnknownVarError::new_box_err(&var_name))
        }
    }

    pub(crate) fn eval_form_deref(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);
        let avalues = self.compile_form_args_nonnil(lctx, tail)?;

        let deref_val =
            builder.build_load(avalues[0].vm_val.into_pointer_value(), "");
        let type_anno = avalues[0]
            .type_anno
            .clone()
            .replace_addr_mode(AddrMode::Value);

        Ok(FormValue::new(
            builder,
            Some(AValue {
                type_anno,
                vm_val: deref_val,
            }),
        ))
    }

    pub(crate) fn eval_form_deref_attr(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let attr_form_value = self.eval_form_attr(lctx.clone(), "", tail)?;

        let attr_value: AValue = attr_form_value.avalue_opt.clone().unwrap();

        let builder = attr_form_value.builder;

        let deref_val =
            builder.build_load(attr_value.vm_val.into_pointer_value(), "");

        let type_anno = attr_value
            .type_anno
            .clone()
            .replace_addr_mode(AddrMode::Value);

        Ok(FormValue::new(
            builder,
            Some(AValue {
                type_anno,
                vm_val: deref_val,
            }),
        ))

        // let deref_tail = list!( list!(sym!(FORM_ATTR), *tail), nil!());
    }

    pub(crate) fn eval_form_let(
        &mut self,
        mut lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let tail: NonNilListData = tail.try_into()?;
        let binds_bracket_tuple: BracketTupleData =
            tail.head.clone().try_into()?;

        let local_block = self
            .ctx
            .vmctx
            .append_basic_block(self.ctx.current_fn.unwrap(), "");

        builder.build_unconditional_branch(local_block);

        let mut sub_lctx =
            lctx.sub_empty(LocalContextType::NestedBody(local_block));

        for mut chunk in &binds_bracket_tuple.items.into_iter().chunks(2) {
            let var_any = chunk.next().unwrap();
            let val_any =
                chunk.next().ok_or(MalformedSyntaxError::new_box_err(""))?;

            let var_sym: SymData = var_any.try_into()?;

            let val = match val_any {
                AnyData::Pri(pri) => match pri {
                    PriData::Lit(lit) => self.compile_lsp_lit(lit),
                    PriData::Sym(sym) => {
                        self.ctx.try_get_avalue(&sub_lctx, &sym.val)?.clone()
                    }
                    _ => unimplemented!(),
                },
                AnyData::Agg(agg) => match agg {
                    AggData::List(list) => self
                        .eval_form(sub_lctx.clone(), list.try_into()?)?
                        .try_into()?,
                    _ => unimplemented!(),
                },
            };

            self.ctx.insert_avalue(&mut sub_lctx, &var_sym.val, val);
        }

        let ret_form = self.eval_form(sub_lctx, tail)?;

        ret_form
            .builder
            .build_unconditional_branch(lctx.get_basic_block());

        Ok(ret_form)
    }

    pub(crate) fn eval_form_progn(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let tail_vec = tail.flatten();

        let mut ret_res = Err(MalformedSyntaxError::new_box_err(
            "progn require at least one form",
        ));

        for elem_any in tail_vec.into_iter() {
            let elem_list: ListData = elem_any.try_into()?;

            ret_res = Ok(self.eval_form(lctx.clone(), elem_list.try_into()?)?);
        }

        ret_res
    }

    pub(crate) fn eval_form_if(
        &mut self,
        mut lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let if_list = tail.flatten();

        if if_list.len() != 3 {
            return Err(MalformedSyntaxError::new_box_err(&format!(
                "if expect 3-clause, however, found {}",
                if_list.len()
            )));
        }

        let builder = lctx.get_builder(&self.ctx.vmctx);

        let form_avalue: AValue = self
            .eval_any(lctx.clone(), if_list[0].clone())?
            .try_into()?;

        let fn_val = self.ctx.current_fn.unwrap();

        let then_block = self.ctx.vmctx.append_basic_block(fn_val, "then");
        let else_block = self.ctx.vmctx.append_basic_block(fn_val, "else");
        let cont_block = self.ctx.vmctx.append_basic_block(fn_val, "cont"); // continue

        builder.build_conditional_branch(
            form_avalue.vm_val.into_int_value(),
            then_block,
            else_block,
        );

        let then_block = builder.get_insert_block().unwrap();
        let then_lctx =
            lctx.sub_empty(LocalContextType::NestedBody(then_block));

        let else_lctx =
            lctx.sub_empty(LocalContextType::NestedBody(else_block));

        let form_then = self.eval_any(then_lctx, if_list[1].clone())?;
        form_then.builder.build_unconditional_branch(cont_block);

        let form_else = self.eval_any(else_lctx, if_list[2].clone())?;
        form_else.builder.build_unconditional_branch(cont_block);

        builder.position_at_end(cont_block);

        let avalue_opt = if let Some(ret_avalue) = form_then.avalue_opt.clone()
        {
            let phi_ret = builder.build_phi(ret_avalue.vm_val.get_type(), "");

            let then_vm_spec_val =
                self.specific_avalue(&form_then.try_into()?)?;
            let else_vm_spec_val =
                self.specific_avalue(&form_else.try_into()?)?;

            phi_ret.add_incoming(&[
                (&*then_vm_spec_val, then_block),
                (&*else_vm_spec_val, else_block),
            ]);

            Some(AValue {
                type_anno: ret_avalue.type_anno.clone(),
                vm_val: phi_ret.as_basic_value(),
            })
        } else {
            None
        };

        let ret_form = FormValue {
            builder,
            avalue_opt,
        };

        Ok(ret_form)
    }

    pub(crate) fn eval_form_create_struct(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let rem: NonNilListData = tail.try_into()?;

        let struct_name_sym: SymData = rem.head.try_into()?;

        if let Some(struct_t) =
            self.ctx.struct_map.get(&struct_name_sym.val).cloned()
        {
            let avalues = self.compile_form_args_nonnil(lctx, rem.tail)?;
            let vm_values: Vec<BasicValueEnum> = avalues
                .into_iter()
                .map(|avalue| avalue.vm_val.into())
                .collect_vec();
            let struct_val =
                self.ctx.vmctx.const_struct(&vm_values[..], false);

            let struct_ptr = builder.build_malloc(struct_t, "")?;
            builder.build_store(struct_ptr, struct_val);

            Ok(FormValue::new(
                builder,
                Some(AValue {
                    type_anno: ConcreteTypeAnno::Struct(
                        struct_name_sym.val.clone(),
                        AddrMode::Value,
                    ),
                    vm_val: struct_ptr.into(),
                }),
            ))
        } else {
            return Err(UnknownStructError::new_box_err(&struct_name_sym.val));
        }
    }

    pub(crate) fn eval_form_add(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let avalues =
            self.compile_form_args_nonnil(lctx.clone(), tail.clone())?;

        if avalues.len() != 2 {
            return Err(UnmatchedFunctionArgsNumberError::new_box_err(
                &format!(
                    "add/sub function args number should be at least two, only {} found",
                    avalues.len()
                )
            ));
        }

        // (add ptr usize)
        let avalue = if avalues[0].type_anno.is_ptr()
            && avalues[1].type_anno.is_int_val()
        {
            let vm_base_ptr = avalues[0].vm_val.into_pointer_value();

            let size = self
                .compile_type_anno(&avalues[0].type_anno)?
                .size_of()
                .unwrap();
            let offset = builder.build_int_mul(
                size,
                avalues[1].vm_val.into_int_value(),
                "",
            );
            let base_ptr_val = builder.build_ptr_to_int(
                vm_base_ptr,
                self.ctx.usize_type(),
                "",
            );

            let res_ptr_val = builder.build_int_add(base_ptr_val, offset, "");
            let res_ptr = builder.build_int_to_ptr(
                res_ptr_val,
                vm_base_ptr.get_type(),
                "",
            );

            AValue {
                type_anno: avalues[0].type_anno.clone(),
                vm_val: res_ptr.into(),
            }
        }
        // (add num num)
        else if avalues[0].type_anno.is_num_val()
            && avalues[1].type_anno.is_num_val()
        {
            let ret = if avalues[0].type_anno.is_int_val() {
                if !avalues[0].type_anno.is_int_val() {
                    return Err(ExplicitCastRequiredError::new_box_err(
                        &format!(
                            "{:?}, {:?}",
                            avalues[0].type_anno, avalues[1].type_anno
                        ),
                    ));
                }

                builder
                    .build_int_add(
                        avalues[0].vm_val.into_int_value(),
                        avalues[1].vm_val.into_int_value(),
                        "",
                    )
                    .into()
            } else {
                if !avalues[0].type_anno.is_float_val() {
                    return Err(ExplicitCastRequiredError::new_box_err(
                        &format!(
                            "{:?}, {:?}",
                            avalues[0].type_anno, avalues[1].type_anno
                        ),
                    ));
                }

                builder
                    .build_float_add(
                        avalues[0].vm_val.into_float_value(),
                        avalues[1].vm_val.into_float_value(),
                        "",
                    )
                    .into()
            };

            let ret_type_anno = if avalues[0].type_anno.num_len()
                < avalues[1].type_anno.num_len()
            {
                avalues[1].type_anno.clone()
            } else {
                avalues[0].type_anno.clone()
            };

            AValue {
                type_anno: ret_type_anno,
                vm_val: ret,
            }
        } else {
            unimplemented!()
        };

        Ok(FormValue::new(builder, Some(avalue)))
    }

    /// (malloc Type usize)
    pub(crate) fn eval_form_malloc(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let rem = tail.flatten();

        let type_anno_map: BraceMapData = rem[0].try_into()?;
        let type_anno =
            parse_concrete_type_anno_map(&self.ctx, type_anno_map)?;
        let vm_type = self.compile_type_anno(&type_anno)?;

        let cap: AValue = self.eval_any(lctx, rem[1].clone())?.try_into()?;

        let ptr = builder.build_array_malloc(
            vm_type,
            cap.vm_val.into_int_value(),
            "",
        )?;

        Ok(FormValue::new(
            builder,
            Some(AValue {
                type_anno,
                vm_val: ptr.into(),
            }),
        ))
    }

    /// (realloc ptr old_size new_size)
    pub(crate) fn eval_form_realloc(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let args_vec = self.compile_form_args_nonnil(lctx, tail)?;

        should_be_3clause(FORM_REALLOC, &args_vec[..])?;

        let old_ptr_avalue = should_be_ptr(FORM_REALLOC, &args_vec[..], 0)?;
        let vm_t =
            self.compile_type_anno(&old_ptr_avalue.type_anno.get_val_type())?;
        let old_ptr = old_ptr_avalue.vm_val.into_pointer_value();

        let old_size = should_be_unsigned_int(FORM_REALLOC, &args_vec[..], 1)?
            .vm_val
            .into_int_value();
        let new_size = should_be_unsigned_int(FORM_REALLOC, &args_vec[..], 2)?
            .vm_val
            .into_int_value();

        let new_ptr = builder.build_array_malloc(vm_t, new_size, "")?;

        builder.build_memmove(new_ptr, 16, old_ptr, 16, old_size)?;

        // Assume that malloc/realloc operate on Unique<Ptr>
        builder.build_free(old_ptr);

        Ok(FormValue::new(
            builder,
            Some(AValue {
                type_anno: old_ptr_avalue.type_anno,
                vm_val: new_ptr.into(),
            }),
        ))
    }

    /// (update-attr ptr [attr1 attr1-attr2 attr1-attr2-attr3] value)
    pub(crate) fn eval_form_update_attr(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);
        let tail: NonNilListData = tail.try_into()?;

        let struct_ptr_any = tail.head.clone();
        let struct_ptr_avalue: AValue<'ctx> =
            self.eval_any(lctx.clone(), struct_ptr_any)?.try_into()?;

        let tail: NonNilListData = tail.tail.try_into()?;
        let ordered_attrs_tuple: BracketTupleData = tail.head.try_into()?;

        let mut ordered_attrs_name = vec![];
        for attr_any in ordered_attrs_tuple.items.into_iter() {
            let attr_sym: SymData = attr_any.try_into()?;
            ordered_attrs_name.push(attr_sym.val);
        }

        let tail: NonNilListData = tail.tail.try_into()?;
        let elem_any = tail.head;
        let elem_avalue: AValue<'ctx> =
            self.eval_any(lctx.clone(), elem_any)?.try_into()?;

        // find final ptr
        let (mut ptr, mut type_anno) = (
            struct_ptr_avalue.vm_val.into_pointer_value(),
            struct_ptr_avalue.type_anno,
        );

        for attr_name in ordered_attrs_name.into_iter() {
            let astruct =
                self.ctx.form_struct_map.get(type_anno.type_name()).unwrap();
            let field_idx = astruct.index_of_field(&attr_name).unwrap();

            ptr = builder.build_struct_gep(ptr, field_idx, "").unwrap();
            type_anno = astruct.fields[field_idx as usize].type_anno.clone();
        }

        builder.build_store(ptr, elem_avalue.vm_val);

        Ok(FormValue {
            builder,
            avalue_opt: None,
        })
    }

    /// (update-index ptr [idx-1 idx1-idx2 idx1-idx2-idx3] value)
    pub(crate) fn eval_form_update_index(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);
        let tail: NonNilListData = tail.try_into()?;

        let struct_ptr_any = tail.head.clone();
        let struct_ptr_avalue: AValue<'ctx> =
            self.eval_any(lctx.clone(), struct_ptr_any)?.try_into()?;

        let tail: NonNilListData = tail.tail.try_into()?;
        let ordered_idxs_tuple: BracketTupleData = tail.head.try_into()?;

        let mut ordered_indexes: Vec<IntValue<'ctx>> = vec![];
        for idx_any in ordered_idxs_tuple.items.into_iter() {
            let idx_avalue: AValue<'ctx> =
                self.eval_any(lctx.clone(), box idx_any)?.try_into()?;
            ordered_indexes.push(idx_avalue.vm_val.into_int_value());
        }

        let tail: NonNilListData = tail.tail.try_into()?;
        let elem_any = tail.head;
        let elem_avalue: AValue<'ctx> =
            self.eval_any(lctx.clone(), elem_any)?.try_into()?;

        // find final ptr
        let mut ptr = struct_ptr_avalue.vm_val.into_pointer_value();

        unsafe {
            ptr = builder.build_in_bounds_gep(ptr, &ordered_indexes, "");
            builder.build_store(ptr, elem_avalue.vm_val);
        }

        Ok(FormValue {
            builder,
            avalue_opt: None,
        })
    }

    /// (not num)
    pub(crate) fn eval_form_not(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let mut avalues = self.compile_form_args_nonnil(lctx, tail)?;

        let elem = avalues.pop().unwrap();

        if elem.type_anno.is_int_val() {
            let ret_vmval =
                builder.build_not(elem.vm_val.into_int_value(), "").into();

            Ok(FormValue {
                builder,
                avalue_opt: Some(AValue {
                    vm_val: ret_vmval,
                    type_anno: ConcreteTypeAnno::bool(),
                }),
            })
        } else {
            Err(UnsupportedIntMathOpError::new_box_err(&format!(
                "not : int, found: {:?}",
                elem.type_anno
            )))
        }
    }

    /// (not num)
    pub(crate) fn eval_form_gt(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let mut avalues = self.compile_form_args_nonnil(lctx, tail)?;

        if avalues.len() != 2 {
            return Err(MalformedSyntaxError::new_box_err(&format!(
                "gt expect 2-clause, however, found {}",
                avalues.len()
            )));
        }

        let rh = avalues.pop().unwrap();
        let lf = avalues.pop().unwrap();

        if lf.type_anno.is_num_val() && rh.type_anno.is_num_val() {
            let ret_vmval =
                if lf.type_anno.is_int_val() && rh.type_anno.is_int_val() {
                    let op = if lf.type_anno.is_unsigned()
                        && rh.type_anno.is_unsigned()
                    {
                        inkwell::IntPredicate::UGT
                    } else {
                        inkwell::IntPredicate::SGT
                    };

                    builder
                        .build_int_compare(
                            op,
                            lf.vm_val.into_int_value(),
                            rh.vm_val.into_int_value(),
                            "",
                        )
                        .into()
                } else if lf.type_anno.is_float_val()
                    && rh.type_anno.is_float_val()
                {
                    let op = inkwell::FloatPredicate::UGT;

                    builder
                        .build_float_compare(
                            op,
                            lf.vm_val.into_float_value(),
                            rh.vm_val.into_float_value(),
                            "",
                        )
                        .into()
                } else {
                    return Err(ExplicitCastRequiredError::new_box_err("gt"));
                };

            Ok(FormValue {
                builder,
                avalue_opt: Some(AValue {
                    vm_val: ret_vmval,
                    type_anno: ConcreteTypeAnno::bool(),
                }),
            })
        } else {
            Err(UnsupportedOpTypeError::new_box_err(&format!(
                "gt: (num, num), found: ({:?}, {:?})",
                lf.type_anno, rh.type_anno
            )))
        }
    }

    /// (mul num num)
    pub(crate) fn eval_form_mul(
        &mut self,
        lctx: LocalContext<'ctx>,
        _name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let mut avalues = self.compile_form_args_nonnil(lctx, tail)?;

        if avalues.len() != 2 {
            return Err(MalformedSyntaxError::new_box_err(&format!(
                "gt expect 2-clause, however, found {}",
                avalues.len()
            )));
        }

        let rh = avalues.pop().unwrap();
        let lf = avalues.pop().unwrap();

        if lf.type_anno.is_num_val() && rh.type_anno.is_num_val() {
            let type_anno = if lf.type_anno.num_len().unwrap()
                > rh.type_anno.num_len().unwrap()
            {
                lf.type_anno.clone()
            } else {
                rh.type_anno.clone()
            };

            let ret_vmval =
                if lf.type_anno.is_int_val() && rh.type_anno.is_int_val() {
                    builder
                        .build_int_mul(
                            lf.vm_val.into_int_value(),
                            rh.vm_val.into_int_value(),
                            "",
                        )
                        .into()
                } else if lf.type_anno.is_float_val()
                    && rh.type_anno.is_float_val()
                {
                    builder
                        .build_float_mul(
                            lf.vm_val.into_float_value(),
                            rh.vm_val.into_float_value(),
                            "",
                        )
                        .into()
                } else {
                    return Err(ExplicitCastRequiredError::new_box_err("gt"));
                };

            Ok(FormValue {
                builder,
                avalue_opt: Some(AValue {
                    vm_val: ret_vmval,
                    type_anno,
                }),
            })
        } else {
            Err(UnsupportedOpTypeError::new_box_err(&format!(
                "gt: (num, num), found: ({:?}, {:?})",
                lf.type_anno, rh.type_anno
            )))
        }
    }

    pub(crate) fn compile_form_args_nonnil(
        &mut self,
        lctx: LocalContext<'ctx>,
        tail: Box<ListData>,
    ) -> Result<Vec<AValue<'ctx>>, Box<dyn Error>> {
        let mut args = tail.flatten();

        let avalue_opts =
            self.compile_form_args(lctx.clone(), &args.make_contiguous()[..])?;

        let mut avalues = vec![];
        for (i, avalue_opt) in avalue_opts.into_iter().enumerate() {
            if avalue_opt.is_none() {
                return Err(VoidAsArgError::new_box_err(
                    format!("{:#?}", args[i]).as_str(),
                ));
            }

            avalues.push(avalue_opt.unwrap())
        }

        Ok(avalues)
    }

    pub(crate) fn compile_form_args(
        &mut self,
        lctx: LocalContext<'ctx>,
        args: &[&Box<AnyData>],
    ) -> Result<Vec<Option<AValue<'ctx>>>, Box<dyn Error>> {
        let mut avalue_opts = vec![];

        for arg in args {
            let avalue_opt = match &***arg {
                AnyData::Pri(pri) => Some(match pri {
                    PriData::Lit(lit) => match lit {
                        LitData::Int(int) => {
                            let (vm_val, type_anno) = if int.len == 8 {
                                let vm_val = self
                                    .ctx
                                    .vmctx
                                    .i64_type()
                                    .const_int(int.val as u64, int.signed)
                                    .into();
                                let type_anno_string =
                                    if int.signed { "i64" } else { "u64" }
                                        .to_owned();

                                let type_anno = ConcreteTypeAnno::Primitive(
                                    type_anno_string,
                                    AddrMode::Value,
                                );

                                (vm_val, type_anno)
                            } else if int.len == 4 {
                                let vm_val = self
                                    .ctx
                                    .vmctx
                                    .i64_type()
                                    .const_int(int.val as u64, int.signed)
                                    .into();
                                let type_anno_string =
                                    if int.signed { "i32" } else { "u32" }
                                        .to_owned();

                                let type_anno = ConcreteTypeAnno::Primitive(
                                    type_anno_string,
                                    AddrMode::Value,
                                );
                                (vm_val, type_anno)
                            } else if int.len == 1 {
                                let vm_val = self
                                    .ctx
                                    .vmctx
                                    .i64_type()
                                    .const_int(int.val as u64, int.signed)
                                    .into();
                                let type_anno_string =
                                    if int.signed { "i8" } else { "u8" }
                                        .to_owned();

                                let type_anno = ConcreteTypeAnno::Primitive(
                                    type_anno_string,
                                    AddrMode::Value,
                                );
                                (vm_val, type_anno)
                            } else {
                                unimplemented!()
                            };

                            AValue { type_anno, vm_val }
                        }
                        LitData::Float(float) => {
                            let vm_val = self
                                .ctx
                                .vmctx
                                .f64_type()
                                .const_float(float.val)
                                .into();
                            let type_anno = ConcreteTypeAnno::Primitive(
                                "float".to_owned(),
                                AddrMode::Value,
                            );

                            AValue { type_anno, vm_val }
                        }
                        LitData::Str(_str) => {
                            // let vm_val = self.ctx.vmctx.const_string(str.val.as_bytes(), true).into();
                            // let type_anno = ConcreteTypeAnno::Primitive("str".to_owned(), AddrMode::Value);

                            // AValue { type_anno, vm_val }
                            unimplemented!()
                        }
                    },
                    PriData::Sym(sym) => {
                        if let Some(avalue) =
                            self.ctx.get_avalue(&lctx, &sym.val)
                        {
                            avalue.clone()
                        } else {
                            return Err(UnknownVarError::new_box_err(
                                &sym.val,
                            ));
                        }
                    }
                    _ => unimplemented!(),
                }),
                AnyData::Agg(agg) => match agg {
                    AggData::List(list) => match list {
                        ListData::Nil(_) => None,
                        ListData::NonNil(nonnillist) => {
                            self.eval_form(lctx.clone(), nonnillist.clone())?
                                .avalue_opt
                        }
                    },
                    _ => unimplemented!(),
                },
            };

            avalue_opts.push(avalue_opt);
        }

        Ok(avalue_opts)
    }

    pub(crate) fn eval_norm_form(
        &mut self,
        lctx: LocalContext<'ctx>,
        name: &str,
        tail: Box<ListData>,
    ) -> Result<FormValue<'ctx>, Box<dyn Error>> {
        let builder = lctx.get_builder(&self.ctx.vmctx);

        let avalues = self.compile_form_args_nonnil(lctx, tail)?;

        let concrete_types = avalues
            .iter()
            .map(|avalue| (*avalue).type_anno.clone())
            .collect_vec();
        let args = avalues
            .iter()
            .map(|avalue| avalue.vm_val.clone().into())
            .collect_vec();

        let concrete_name = concat_overload_name(
            name,
            &concrete_types[..],
            NameConcatStyle::Fn,
        );

        let fnval =
            if let Some(fnval) = self.ctx.vmmod.get_function(&concrete_name) {
                fnval
            } else if let Some(template_fn) =
                self.ctx.template_fn_map.get(name).cloned()
            {
                let afn =
                    template_fn.expand(&mut self.ctx, &concrete_types[..])?;
                self.compile_declare(&afn, None)?;
                self.compile_definition(&afn)?;

                self.ctx.vmmod.get_function(&concrete_name).unwrap()
            } else {
                return Err(UnknownFunctionError::new_box_err(&concrete_name));
            };

        let ret = builder.build_call(fnval, &args[..], "");

        let afn = self.ctx.form_fn_map.get(&concrete_name).unwrap();

        let ret = match afn.ret {
            Some(ref type_anno) => Some(AValue {
                type_anno: type_anno.clone(),
                vm_val: ret.try_as_basic_value().unwrap_left().clone(),
            }),
            None => None,
        };

        Ok(FormValue::new(builder, ret))
    }
}

/// Load recursively
fn load_file(
    path: &Path,
    ctx: &mut CompileContext,
) -> Result<(), Box<dyn Error>> {
    let config = LispParserConfig::default();
    // let config = LispParserConfig {
    //     tokens_output: Some(PrintTy::StdErr),
    // };

    let mut parser = LispParser::new(path, config)?;
    let lm = parser.parse()?;

    parse_phase_2::parse(lm, ctx)?;

    Ok(())
}
