use std::error::Error;

use bacommon::error::{UnknownGenericNameError, UnknownTemplateStructError, UnmatchedGenericNumberError};
use convert_case::{Case, Casing};
use inkwell::types::BasicTypeEnum;
use itertools::Itertools;
use lisparser::data::*;
use lisparser::*;

use crate::{core_syntax::{a_struct::ConcreteField}, parse_phase_2::parse_generic_name};

use super::{CompileContext, ConcreteTypeAnno, a_fn::AFn, name_mangling::{concat_overload_name, NameConcatStyle}, type_anno::TypeAnno};
use crate::core_syntax::r#const::*;


////////////////////////////////////////////////////////////////////////////////
//// `TemplateFn` Form

#[allow(unused)]
#[derive(Debug, Clone)]
pub(crate) struct TemplateFn {
    pub(crate) name: String,
    pub(crate) generic_names: Vec<String>,
    pub(crate) params: Vec<Param>,
    pub(crate) ret: Option<TypeAnno>,
    pub(crate) body: ListData,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub(crate) struct Param {
    pub(crate) formal: String,
    pub(crate) type_anno: TypeAnno,
}

impl<'ctx> TemplateFn {
    pub(crate) fn expand(
        &self,
        ctx: &mut CompileContext<'ctx>,
        concrete_types: &[ConcreteTypeAnno],
    ) -> Result<AFn, Box<dyn Error>> {
        if self.generic_names.len() != concrete_types.len() {
            return Err(UnmatchedGenericNumberError::new_box_err(&format! {
                "expect {}, found {}",
                self.generic_names.len(),
                concrete_types.len()
            }));
        }

        let concrete_name = concat_overload_name(
            &self.name,
            &concrete_types,
            NameConcatStyle::Fn,
        );

        // load from memory cache
        if let Some(afn) = ctx.form_fn_map.get(&concrete_name) {
            return Ok(afn.clone());
        }

        // expand
        let mut concrete_params = Vec::with_capacity(self.params.len());
        for param in self.params.iter() {
            let type_anno = param.type_anno.finalize(ctx, concrete_types)?;
            let formal = param.formal.to_owned();

            concrete_params.push(ConcreteField { formal, type_anno })
        }
        let ret = if let Some(ref ret_type_anno) = self.ret {
            Some(ret_type_anno.finalize(ctx, concrete_types)?)
        } else {
            None
        };
        let body = self.expand_form(ctx, self.body.clone(), concrete_types)?;


        let afn = AFn {
            name: concrete_name.clone(),
            params: concrete_params,
            body,
            ret,
        };

        ctx.form_fn_map.insert(concrete_name, afn.clone());

        Ok(afn)
    }

    fn index_of_generic_name(&self, generic_name: &str) -> Option<usize> {
        if let Some((idx, _)) = self
        .generic_names
        .iter()
        .find_position(|&x| x == generic_name) {
            Some(idx)
        }
        else {
            None
        }
    }

    fn expand_form(
        &self,
        ctx: &mut CompileContext<'ctx>,
        list: ListData,
        concrete_types: &[ConcreteTypeAnno],
    ) -> Result<ListData, Box<dyn Error>> {
        Ok(match list {
            ListData::Nil(nil) => nil.into(),
            ListData::NonNil(nonnil) => {
                let head_sym: SymData = nonnil.head.try_into()?;
                let tail: NonNilListData = nonnil.tail.try_into()?;

                if &head_sym.val == FORM_TEMPLATE_CREATE_STRUCT {
                    let generic_names_tuple: BracketTupleData =
                        tail.head.try_into()?;
                    let generic_names = parse_generic_name(generic_names_tuple)?;
                    let mut collected_concrete_type_names = vec![];

                    for generic_name in generic_names.iter() {
                        if let Some(idx) = self.index_of_generic_name(generic_name) {
                            collected_concrete_type_names.push(
                                 concrete_types[idx].clone()
                            );
                        }
                        else {
                            return Err(
                                UnknownGenericNameError::new_box_err(
                                    &format!(
                                        "{:?} found {}", self.generic_names, generic_name
                                    )
                                )
                            )
                        }
                    }

                    let tail: NonNilListData = tail.tail.try_into()?;

                    let struct_sym: SymData = tail.head.try_into()?;

                    let template_struct = match ctx.template_struct_map.get(&struct_sym.val) {
                        Some(template_struct) => template_struct.clone(),
                        None => return Err(
                            UnknownTemplateStructError::new_box_err(
                                &struct_sym.val
                            )
                        )
                    };

                    let astruct = template_struct.expand(ctx, concrete_types)?;

                    let rem_list = self.expand_form(ctx, *tail.tail, concrete_types)?;

                    list!(
                        sym!(FORM_CREATE_STRUCT),
                        list!(
                            sym!(&astruct.name),
                            rem_list
                        )
                    )
                }
                else {
                    /* Normal Form Template Type Simply Replace */

                    let mut collected_args = vec![];

                    for arg in tail.flatten().into_iter() {
                        let any_data: AnyData = match *arg.clone() {
                            AnyData::Pri(pri) => match pri {
                                PriData::Lit(lit) => lit.into(),
                                PriData::Key(key) => key.into(),
                                PriData::Sym(sym) => {
                                    if let Some(idx) = self.index_of_generic_name(&sym.val) {
                                        concrete_types[idx].unparse().into()
                                    }
                                    else {
                                        sym.into()
                                    }
                                },
                            },
                            AnyData::Agg(agg) => match agg {
                                AggData::List(list) => self.expand_form(ctx, list.clone(), concrete_types)?.into(),
                                _ => unimplemented!()
                            },
                        };

                        collected_args.push(any_data)
                    }



                    ListData::deflatten(&collected_args[..])
                }

            }
        })
    }
}
