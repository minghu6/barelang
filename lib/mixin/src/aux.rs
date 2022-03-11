#![allow(unused)]

use std::ffi::CString;


pub(crate) fn cstr_p(s: &str) -> *mut i8 {
    CString::new(s).unwrap().into_raw()
}

pub(crate) fn cstr_u(s: &str) -> usize {
    cstr_p(s) as usize
}
