use std::{error::Error, path::{Path, PathBuf}};

use itertools::Itertools;
use m6stack::AnIteratorWrapper;
use lisparser::{data::{BracketTupleData, NonNilListData, SymData}, parser::LispParser};


use crate::{error::*, parse_phase_2};

use super::{CompileContext, spec_etc::{extract_index_path_from_dir, name_to_path}};

////////////////////////////////////////////////////////////////////////////////
//// `NS` Form

pub(crate) struct NS {
    pub(crate) name: String,      // namespace name
    pub(crate) mods: Vec<String>, // modules name belongs to the namespace
    pub(crate) path: PathBuf
}

impl NS {
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

    pub(crate) fn load(&self, ctx: &mut CompileContext) -> Result<(), Box<dyn Error>> {
        for sub in self.sub_paths() {
            load_file(&sub, ctx)?;
        }

        Ok(())
    }
}


impl TryFrom<&Path> for NS {
    type Error = Box<dyn Error>;

    fn try_from(dir: &Path) -> Result<Self, Self::Error> {
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

                NS {
                    name,
                    mods,
                    path: dir.to_owned(),
                }
            },
            other => return Err(XXXError::new_box_err(other))
        })

    }
}

/// Load recursively
pub fn load_file(path: &Path, ctx: &mut CompileContext) -> Result<(), Box<dyn Error>> {
    let mut parser = LispParser::new(path)?;
    let lm = parser.parse()?;

    parse_phase_2::parse(lm, ctx)?;

    Ok(())
}

