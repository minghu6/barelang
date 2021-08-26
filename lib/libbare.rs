

#[no_mangle]
pub extern "C" fn printi(x: i32) -> i32 {
    println!("rust{}rust", x);
    x
}

#[no_mangle]
pub extern "C" fn printi32(x: i32) -> i32 {
    println!("rust{}rust", x);
    x
}
