use std::fmt::{self, Display};

use crate::data::{AggData, AnyData, BraceMapData, BracketTupleData, LispModule, ListData};


#[allow(unused)]
pub struct PrettyPrintConfig {
    pub(crate) maxcol: usize
}




////////////////////////////////////////////////////////////////////////////////
//// Indent

#[derive(Debug, Clone)]
pub struct Indent {
    pub unit: String,
    pub level: usize
}


impl Indent {
    pub fn new(unit: &str, level: usize) -> Self {
        Self { level, unit: unit.to_owned() }
    }

    pub fn push(&self) -> Self {
        let mut cloned = self.clone();
        cloned.level += 1;
        cloned
    }

    pub fn pop(&mut self) -> Self {
        let mut cloned = self.clone();
        cloned.level -= 1;
        cloned
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.unit.repeat(self.level))
    }
}

impl Default for Indent {
    fn default() -> Self {
        Self::new(" ", Default::default())
    }
}



////////////////////////////////////////////////////////////////////////////////
//// Dump

pub trait Dump {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result;
}


impl Display for dyn Dump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f, Indent::default())
    }
}


impl Dump for ListData {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        let items = self.flatten();

        match items.len() {
            0 => write!(f, "{}", "nil")?,
            1 => {
                write!(f, "{}", "(")?;
                items[0].dump(f, indent)?;
                write!(f, "{}", ")")?;
            }
            _ => {
                write!(f, "{}", "(")?;
                items[0].dump(f, indent.clone())?;

                for item_rem in items.into_iter().skip(1) {
                    writeln!(f)?;
                    write!(f, "{}", indent)?;
                    item_rem.dump(f, indent.clone())?;
                }

                write!(f, "{}", ")")?;
            }
        }

        Ok(())
    }
}


impl Dump for AnyData {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        match &self {
            Self::Agg(agg) => agg.dump(f, indent),
            Self::Pri(pri) => write!(f, "{}", pri)
        }
    }
}

impl Dump for LispModule {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        writeln!(f, "{:#?}", self.meta)?;

        for list in self.lists.iter() {
            list.dump(f, indent.push())?;
            writeln!(f)?;
            writeln!(f)?;
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Dump for AggData {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        match &self {
            Self::List(list) => list.dump(f, indent),
            Self::BracketTuple(bt) => bt.dump(f, indent),
            Self::BraceMap(bm) => bm.dump(f, indent),
        }
    }
}

impl Dump for BracketTupleData {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        match self.items.len() {
            0 => write!(f, "{}", "[]")?,
            1 => {
                write!(f, "{}{}", indent.push(), "[")?;
                self.items[0].dump(f, indent.push())?;
                write!(f, "{}", "]")?;
            }
            _ => {
                write!(f, "{}{}", indent.push(), "[")?;
                self.items[0].dump(f, indent.push())?;

                for item_rem in self.items.iter().skip(1) {
                    writeln!(f)?;
                    write!(f, "{}", indent.push())?;
                    item_rem.dump(f, indent.push())?;
                }

                write!(f, "{}", "]")?;
            }
        }

        Ok(())
    }
}


impl Dump for BraceMapData {
    fn dump(&self, f: &mut fmt::Formatter<'_>, indent: Indent) -> fmt::Result {
        match self.entries.len() {
            0 => write!(f, "{}", "{}")?,
            1 => {
                write!(f, "{}", "{")?;

                dump_entry(f, indent.push(), &self.entries[0])?;

                write!(f, "{}", "}")?;
            }
            _ => {
                write!(f, "{}", "{")?;
                dump_entry(f, indent.push(), &self.entries[0])?;

                for entry in self.entries.iter().skip(1) {
                    writeln!(f)?;
                    dump_entry(f, indent.push(), entry)?;
                }

                write!(f, "{}", "}")?;
            }
        }

        Ok(())
    }
}

fn dump_entry(f: &mut fmt::Formatter<'_>, indent: Indent, entry: &(AnyData, AnyData)) -> fmt::Result {
    let (k, v) = entry;

    k.dump(f, indent.push())?;
    write!(f, "{}", " ")?;
    v.dump(f, indent.push())
}

