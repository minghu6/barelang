

#[no_mangle]
pub extern "C" fn printi(x: i64) -> i64 {
    println!("rust{}rust", x);
    x
}
