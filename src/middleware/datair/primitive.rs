
use super::*;

////////////////////////////////////////////////////////////////////////////////
//// BaFunHdr

#[derive(Debug, Clone)]
pub struct BaFunHdr {
    pub name: BaId,
    pub params: Vec<BaParam>,
    pub ret: BaType,
}

impl BaFunHdr {
    pub fn name(&self) -> String {
        self.name.name.to_owned()
    }
}

impl From<(BaId, Vec<BaParam>, BaType)> for BaFunHdr {
    fn from(triple: (BaId, Vec<BaParam>, BaType)) -> Self {
        Self {
            name: triple.0,
            params: triple.1,
            ret: triple.2,
        }
    }
}

impl BaFunHdr {
    pub fn as_key(&self) -> BaFunKey {
        BaFunKey(
            self.name.name.clone(),
            self.params
                .iter()
                .map(|param| param.ty.clone())
                .collect_vec(),
        )
    }
}

impl GetLoc for BaFunHdr {
    fn get_loc(&self) -> SrcLoc {
        self.name.get_loc()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaParam

#[derive(Debug, Clone)]
pub struct BaParam {
    pub formal: String,
    pub ty: BaType,
}

impl From<BaId> for BaParam {
    fn from(id: BaId) -> Self {
        Self {
            formal: id.name,
            ty: id.ty.unwrap(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaFunKey

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BaFunKey(pub String, pub Vec<BaType>);

////////////////////////////////////////////////////////////////////////////////
//// BaRange

#[derive(Debug, Clone)]
pub struct BaRange {
    pub start: Option<BaPriVal>,
    pub end: Option<BaPriVal>,
    pub srcloc: SrcLoc,
}

impl GetBaType for BaRange {
    fn get_batype(&self) -> Option<BaType> {
        Some(match (&self.start, &self.end) {
            (Some(pri_start), Some(_pri_end)) => {
                BaType::Range(Box::new(pri_start.get_batype().unwrap()))
            },
            (Some(pri_start), None) => {
                BaType::RangeFrom(Box::new(pri_start.get_batype().unwrap()))
            },
            (None, Some(pri_end)) => {
                BaType::RangeTo(Box::new(pri_end.get_batype().unwrap()))
            },
            (None, None) => {
                BaType::RangeFull()
            }
        })
    }
}



////////////////////////////////////////////////////////////////////////////////
//// BaVector

#[derive(Debug, Clone)]
pub enum BaVector {
    Array(BaArray),
}

impl GetBaType for BaVector {
    fn get_batype(&self) -> Option<BaType> {
        match &self {
            Self::Array(barr) => barr.get_batype(),
        }
    }
}

impl GetLoc for BaVector {
    fn get_loc(&self) -> SrcLoc {
        match &self {
            Self::Array(barr) => barr.get_loc(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaArray

#[derive(Debug, Clone)]
pub struct BaArray {
    pub ty: Option<BaType>,
    pub elems: Vec<BaPriVal>,
    pub srcloc: SrcLoc,
}

impl GetBaType for BaArray {
    fn get_batype(&self) -> Option<BaType> {
        if let Some(ty) = &self.ty {
            Some(BaType::Arr(Box::new(ty.clone())))
        } else {
            None
        }
    }
}

impl GetLoc for BaArray {
    fn get_loc(&self) -> SrcLoc {
        self.srcloc.clone()
    }
}

////////////////////////////////////////////////////////////////////////////////
//// BaParameter

pub type BaParameter = BaId;
