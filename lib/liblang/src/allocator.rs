

// use std::os::raw::{
//     c_void, c_int
// };
use std::alloc::{
    alloc_zeroed, Layout
};
use std::ptr::{
    drop_in_place, slice_from_raw_parts_mut
};



/// Heap Allocation
#[no_mangle]
pub unsafe extern "C" fn allocate(bytes: usize) -> *mut u8 {

    let layout = Layout::array::<u8>(bytes).unwrap();

    alloc_zeroed(layout)
}

#[no_mangle]
pub unsafe extern "C" fn deallocate(ptr: *mut u8, len: usize) {
    drop_in_place(slice_from_raw_parts_mut(ptr, len))
}
