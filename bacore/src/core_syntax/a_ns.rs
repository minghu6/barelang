use std::{error::Error, path::{Path, PathBuf}};

use bacommon::{config::CompilerConfig, target_generator::TargetGenerator};
use inkwell::context::Context;
use itertools::Itertools;
use m6stack::AnIteratorWrapper;
use lisparser::{data::{BracketTupleData, NonNilListData, SymData}, parser::LispParser};


use crate::{parse_phase_2};

use bacommon::error::*;
use super::{CompileContext, spec_etc::{extract_index_path_from_dir, name_to_path}};

////////////////////////////////////////////////////////////////////////////////
//// `NS` Form

pub struct ANS<'ctx> {
    pub name: String,      // namespace name
    pub(crate) mods: Vec<String>, // modules name belongs to the namespace
    pub(crate) path: PathBuf,
    pub(crate) ctx: CompileContext<'ctx>
}

impl<'ctx> ANS<'ctx> {
    pub fn init(dir: &Path, vmctx: &'ctx Context) -> Result<Self, Box<dyn Error>> {
        let dirname = dir.file_name().unwrap().to_str().unwrap();
        let ctx = CompileContext::new(dirname, vmctx);
        let index_path= extract_index_path_from_dir(dir);

        let mut parser = LispParser::new(&index_path)?;
        let lm = parser.parse()?;

        let list1st: NonNilListData = lm.lists[0].clone().try_into()?;

        let head_sym: SymData = list1st.head.try_into()?;

        Ok(match head_sym.val.as_str() {
            "ns" => {
                let tail: NonNilListData = list1st.tail.try_into()?;

                let mut tail_iter = tail.flatten().into_iter();

                let ns_name_sym: SymData = tail_iter
                    .next().ok_or(XXXError::new_box_err(""))?
                    .try_into()?;
                let name = ns_name_sym.val;

                let sub_any = tail_iter
                    .next().ok_or(XXXError::new_box_err(""))?;

                let sub_tuple: BracketTupleData = sub_any.try_into()?;
                let mut mods = vec![];

                for item_any in sub_tuple.items.iter() {
                    mods.push(TryInto::<SymData>::try_into(item_any)?.val)
                }

                ANS {
                    name,
                    mods,
                    path: dir.to_owned(),
                    ctx
                }
            },
            other => return Err(XXXError::new_box_err(other))
        })

    }


    pub(crate) fn sub_paths(&self) -> AnIteratorWrapper<PathBuf> {
        let iter = box self.mods
        .iter()
        .map(|sub| {
            let sub_name = name_to_path(sub);
            self.path.join(sub_name)
        });

        AnIteratorWrapper {
            iter
        }
    }


    pub(crate) fn load(&mut self) -> Result<(), Box<dyn Error>> {
        for sub in self.sub_paths().collect_vec().iter() {
            load_file(&sub, &mut self.ctx)?;
        }

        Ok(())
    }


    pub fn print(&self, config: &CompilerConfig) -> Result<(), Box<dyn Error>> {
        let gen = TargetGenerator::new(&self.ctx.vmmod, config);

        gen.print()
    }
}



/// Load recursively
pub fn load_file(path: &Path, ctx: &mut CompileContext) -> Result<(), Box<dyn Error>> {
    let mut parser = LispParser::new(path)?;
    let lm = parser.parse()?;

    parse_phase_2::parse(lm, ctx)?;

    Ok(())
}

