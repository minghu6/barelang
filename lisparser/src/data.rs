//! Lisp Syntax Data Type
#![allow(unused_imports)]

use std::any::type_name;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::path::Path;
use std::rc::Rc;

use crate::error::TryIntoFailed;
use crate::parser::SrcLoc;
use crate::*;


////////////////////////////////////////////////////////////////////////////////
//// Common Traits

pub trait Loc {
    fn loc(&self) -> SrcLoc;
}

////////////////////////////////////////////////////////////////////////////////
//// Module Meta

#[derive(Debug, Clone)]
pub struct ModuleMeta {
    pub path: Box<Path>,
    pub name: String,
}

////////////////////////////////////////////////////////////////////////////////
//// Lisp Module

#[derive(Debug)]
pub struct LispModule {
    pub meta: ModuleMeta,
    pub lists: Vec<ListData>,
}



////////////////////////////////////////////////////////////////////////////////
//// Primary Type

#[derive(Debug, Clone, PartialEq)]
pub enum PriData {
    Lit(LitData),
    Key(KeyData), // :abc
    Sym(SymData),   // (def a 2), a is var ref, ref to 2
}

impl Display for PriData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Lit(lit) => write!(f, "{}", lit),
            Self::Key(key) => write!(f, "{}", key.val),
            Self::Sym(sym) => write!(f, "{}", sym.val),
        }
    }
}

impl Loc for PriData {
    fn loc(&self) -> SrcLoc {
        match self {
            Self::Lit(lit) => lit.loc(),
            Self::Key(key) => key.loc.clone(),
            Self::Sym(id) => id.loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct KeyData {
    pub val: String,
    pub loc: SrcLoc,
}

impl PartialEq for KeyData {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl Into<AnyData> for KeyData {
    fn into(self) -> AnyData {
        AnyData::Pri(PriData::Key(self))
    }
}



#[derive(Debug, Clone)]
pub struct SymData {
    pub val: String,
    pub loc: SrcLoc,
}

impl PartialEq for SymData {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl Into<AnyData> for SymData {
    fn into(self) -> AnyData {
        AnyData::Pri(PriData::Sym(self))
    }
}



////////////////////////////////////////////////////////////////////////////////
//// Lit Type

#[derive(Debug, Clone, PartialEq)]
pub enum LitData {
    Int(IntData),
    Float(FloatData),
    Str(StrData),
}

impl Loc for LitData {
    fn loc(&self) -> SrcLoc {
        match &self {
            Self::Int(int) => int.loc.clone(),
            Self::Float(float) => float.loc.clone(),
            Self::Str(str) => str.loc.clone(),
        }
    }
}

impl Display for LitData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Int(int) => write!(f, "{}", int.val),
            Self::Float(float) => write!(f, "{}", float.val),
            Self::Str(str) => write!(f, "{}", str.val),
        }
    }
}

impl Into<AnyData> for LitData {
    fn into(self) -> AnyData {
        AnyData::Pri(PriData::Lit(self))
    }
}



#[derive(Debug, Clone)]
pub struct IntData {
    pub len: usize,  // bytes number
    pub signed: bool,
    pub val: u128,
    pub loc: SrcLoc,
}

impl PartialEq for IntData {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.signed == other.signed && self.val == other.val
    }
}





#[derive(Debug, Clone)]
pub struct FloatData {
    pub len: usize,
    pub val: f64,
    pub loc: SrcLoc,
}

impl PartialEq for FloatData {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.val == other.val
    }
}


#[derive(Debug, Clone)]
pub struct StrData {
    pub val: String,
    pub loc: SrcLoc,
}

impl PartialEq for StrData {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}


////////////////////////////////////////////////////////////////////////////////
//// Aggregate Type

#[derive(Debug, Clone, PartialEq)]
pub enum AggData {
    List(ListData),                 // (a b c)
    BracketTuple(BracketTupleData), // [a b c]
    BraceMap(BraceMapData),         // {a b c}
}

impl Loc for AggData {
    fn loc(&self) -> SrcLoc {
        match &self {
            Self::List(list) => list.loc(),
            Self::BraceMap(map) => map.loc.clone(),
            Self::BracketTuple(tuple) => tuple.loc.clone(),
        }
    }
}



#[derive(Debug, Clone, PartialEq)]
pub enum ListData {
    Nil(NilData),
    NonNil(NonNilListData),
}

impl Loc for ListData {
    fn loc(&self) -> SrcLoc {
        match &self {
            Self::Nil(nil) => nil.loc.clone(),
            Self::NonNil(nonnil) => nonnil.loc.clone(),
        }
    }
}

impl ListData {
    pub fn is_nil(&self) -> bool {
        if let Self::Nil(_nil) = self {
            true
        } else {
            false
        }
    }

    pub fn flatten(&self) -> VecDeque<&Box<AnyData>> {
        match &self {
            Self::NonNil(nonnil) => {
                nonnil.flatten()
            },
            Self::Nil(_nil) => {
                Default::default()
            }
        }
    }

    pub fn deflatten(anys: &[AnyData]) -> Self {
        if anys.is_empty() {
            return ListData::nil()
        }

        list!(anys[0].clone(), Self::deflatten(&anys[1..]))
    }

    pub fn nil() -> Self {
        Self::Nil(NilData { loc: SrcLoc::default() })
    }

    pub fn try_as_nonnil(self) -> Result<NonNilListData, Box<dyn Error>> {
        match self {
            Self::NonNil(nonnil) => Ok(nonnil),
            _ => Err(box TryIntoFailed::new(
                format!("{:?}", self),
                type_name::<NonNilListData>(),
            ))
        }
    }
}


impl TryInto<NonNilListData> for ListData {
    type Error = Box<dyn Error>;

    fn try_into(self) -> Result<NonNilListData, Self::Error> {
        Self::try_as_nonnil(self)
    }
}

impl Into<AnyData> for ListData {
    fn into(self) -> AnyData {
        AnyData::Agg(AggData::List(self))
    }
}


#[derive(Debug, Clone)]
pub struct NilData {
    pub loc: SrcLoc,
}

impl PartialEq for NilData {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Into<ListData> for NilData {
    fn into(self) -> ListData {
        ListData::Nil(self)
    }
}


#[derive(Debug, Clone)]
pub struct NonNilListData {
    pub head: Box<AnyData>,
    pub tail: Box<ListData>,
    pub loc: SrcLoc,
}

impl NonNilListData {
    pub fn flatten(&self) -> VecDeque<&Box<AnyData>> {
        let mut deq = self.tail.flatten();
        deq.push_front(&self.head);

        deq
    }
}


impl PartialEq for NonNilListData {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail
    }
}

impl TryFrom<Box<ListData>> for NonNilListData {
    type Error = Box<TryIntoFailed>;

    fn try_from(value: Box<ListData>) -> Result<Self, Self::Error> {
        if let ListData::NonNil(nonnil) = *value {
            Ok(nonnil)
        } else {
            Err(box TryIntoFailed::new(
                format!("{:?}", value),
                type_name::<NonNilListData>(),
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct BracketTupleData {
    pub items: Vec<AnyData>,
    pub loc: SrcLoc,
}

impl PartialEq for BracketTupleData {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}



#[derive(Debug, Clone)]
pub struct BraceMapData {
    pub entries: Vec<(AnyData, AnyData)>,
    pub loc: SrcLoc,
}

impl BraceMapData {
    pub fn get(&self, key: &AnyData) -> Option<&AnyData> {
        self.entries
            .iter()
            .find(|(k, _)| k == key)
            .and_then(|(_, v)| Some(v))
    }

    pub fn get_by_keyword(&self, keywordval: &str) -> Option<&AnyData> {
        let key = KeyData { val: keywordval.to_owned(), loc: SrcLoc::default() };

        self.get(&key.into())
    }

    pub fn get_by_symbol(&self, symval: &str) -> Option<&AnyData> {
        let key = SymData { val: symval.to_owned(), loc: SrcLoc::default() };

        self.get(&key.into())
    }
}



impl PartialEq for BraceMapData {
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl Into<AnyData> for BraceMapData {
    fn into(self) -> AnyData {
        AnyData::Agg(AggData::BraceMap(self))
    }
}


////////////////////////////////////////////////////////////////////////////////
//// AnyType

#[derive(Debug, Clone, PartialEq)]
pub enum AnyData {
    Pri(PriData),
    Agg(AggData),
}

impl Loc for AnyData {
    fn loc(&self) -> SrcLoc {
        match &self {
            Self::Agg(agg) => agg.loc(),
            Self::Pri(pri) => pri.loc(),
        }
    }
}

impl AnyData {
    pub(crate) fn nonnil_trivial_list(
        data1st: AnyData,
        data2nd: AnyData,
        loc: SrcLoc,
    ) -> Self {
        AnyData::Agg(AggData::List(ListData::NonNil(NonNilListData {
            head: box data1st,
            tail: box ListData::NonNil(NonNilListData {
                head: box data2nd,
                tail: box ListData::Nil(NilData { loc: loc.clone() }),
                loc: loc.clone(),
            }),
            loc,
        })))
    }
}



////////////////////////////////////////////////////////////////////////////////
//// TryInto SymData

impl TryInto<SymData> for AnyData {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<SymData, Self::Error> {
        let self_msg = format!("{:?}", self);

        match self {
            AnyData::Agg(_agg) => (),
            AnyData::Pri(pri) => match pri {
                PriData::Sym(id) => return Ok(id),
                _ => (),
            },
        };

        Err(box TryIntoFailed::new(self_msg, type_name::<SymData>()))
    }
}

impl TryInto<SymData> for Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<SymData, Self::Error> {
        (*self).try_into()
    }
}

impl TryInto<SymData> for &Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<SymData, Self::Error> {
        (*self.clone()).try_into()
    }
}


impl TryInto<SymData> for &AnyData {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<SymData, Self::Error> {
        self.clone().try_into()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// TryInto BracketTupleData


impl TryInto<BracketTupleData> for AnyData {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BracketTupleData, Self::Error> {
        let self_msg = format!("{:?}", self);

        match self {
            AnyData::Agg(agg) => match agg {
                AggData::BracketTuple(brt) => return Ok(brt),
                _ => ()
            },
            AnyData::Pri(_) => (),
        };

        Err(box TryIntoFailed::new(self_msg, type_name::<BracketTupleData>()))
    }
}

impl TryInto<BracketTupleData> for Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BracketTupleData, Self::Error> {
        (*self).try_into()
    }
}

impl TryInto<BracketTupleData> for &Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BracketTupleData, Self::Error> {
        (*self.clone()).try_into()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// TryInto BraceMapData


impl TryInto<BraceMapData> for AnyData {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BraceMapData, Self::Error> {
        let self_msg = format!("{:?}", self);

        match self {
            AnyData::Agg(agg) => match agg {
                AggData::BraceMap(brm) => return Ok(brm),
                _ => ()
            },
            AnyData::Pri(_) => (),
        };

        Err(box TryIntoFailed::new(self_msg, type_name::<BraceMapData>()))
    }
}

impl TryInto<BraceMapData> for Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BraceMapData, Self::Error> {
        (*self).try_into()
    }
}

impl TryInto<BraceMapData> for &Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<BraceMapData, Self::Error> {
        (*self.clone()).try_into()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// TryInto ListData

impl TryInto<ListData> for AnyData {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<ListData, Self::Error> {
        let self_msg = format!("{:?}", self);

        match self {
            AnyData::Agg(agg) => match agg {
                AggData::List(list) => return Ok(list),
                _ => ()
            },
            AnyData::Pri(_) => (),
        };

        Err(box TryIntoFailed::new(self_msg, type_name::<ListData>()))
    }
}

impl TryInto<ListData> for Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<ListData, Self::Error> {
        (*self).try_into()
    }
}

impl TryInto<ListData> for &Box<AnyData> {
    type Error = Box<TryIntoFailed>;

    fn try_into(self) -> Result<ListData, Self::Error> {
        (*self.clone()).try_into()
    }
}


