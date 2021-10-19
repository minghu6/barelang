use super::*;

////////////////////////////////////////////////////////////////////////////////
//// External C Type

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTy {
    I64,
    I32,
    F64,
    CStr,  // char*
    Void
}

impl CTy {
    pub fn as_type(&self) -> BaType {
        match &self {
            Self::F64 => BaType::Float,
            Self::I64 => BaType::I64,
            Self::I32 => BaType::Int,
            Self::CStr => BaType::RawStr,
            Self::Void => BaType::VoidUnit
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CFun {
    pub name: String,
    pub params: Vec<CTy>,
    pub ret: CTy,
}
