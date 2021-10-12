use std::{ffi::{CStr, CString}, os::raw::c_char};
use std::mem::forget;


#[no_mangle]
pub extern "C" fn printi(x: i32) -> i32 {
    println!("{}", x);
    x
}

#[no_mangle]
pub extern "C" fn printi32(x: i32) -> i32 {
    println!("{}", x);
    x
}

#[no_mangle]
pub extern "C" fn prints(x: *const c_char) -> *const c_char {
    let c_str = unsafe { CStr::from_ptr(x) };
    let s = c_str.to_str().map(|s| s.to_owned()).unwrap();

    println!("{}", s);

    let cs = CString::new(s).unwrap();
    let p = cs.as_ptr();
    forget(cs);

    p
}




#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use std::os::raw::c_char;

        use crate::prints;

        let s = "这是今天的世界";

        prints(s.as_ptr() as *const c_char);
    }
}
