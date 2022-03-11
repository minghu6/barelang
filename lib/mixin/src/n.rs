use std::{
    cmp::Ordering,
    collections::VecDeque,
    ffi::CStr,
    iter::zip,
    mem::size_of,
    ops::{Index, IndexMut},
    ptr::{copy, copy_nonoverlapping, null, null_mut},
    slice::from_raw_parts,
};

use m6arr::Array;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::allocator::{allocate, deallocate};


#[derive(FromPrimitive, ToPrimitive, Clone, Copy)]
pub enum PriTy {
    Int = 0,
    Str,
}

impl PriTy {
    pub fn encode(raw: &[Self]) -> Vec<u8> {
        let mut vec = vec![];

        for pri in raw {
            vec.push(pri.to_u8().unwrap())
        }

        vec
    }

    fn size_of(&self) -> usize {
        size_of::<usize>()
    }

    fn get_cmp(&self) -> Box<dyn Fn(usize, usize) -> Ordering> {
        unsafe {
            match self {
                Self::Int => {
                    box |p1: usize, p2: usize| -> Ordering { p1.cmp(&p2) }
                }
                Self::Str => box |p1: usize, p2: usize| -> Ordering {
                    // println!("p1: {:016x},  {}", p1, *(p1 as *mut u8) as char);
                    // println!("p2: {:016x},  {}", p2, *(p2 as *mut u8) as char);

                    let p1cstr = CStr::from_ptr(p1 as *mut i8);
                    let p2cstr = CStr::from_ptr(p2 as *mut i8);

                    p1cstr.cmp(&p2cstr)
                },
            }
        }
    }

    pub fn as_value(&self) -> u8 {
        self.to_u8().unwrap()
    }
}



////////////////////////////////////////////////////////////////////////////////
//// Mixed Primary Type Vector (NonHomogeneous)

#[repr(C)]
pub struct MixedVec {
    offset: *mut usize, // u128 maybe more compatiable for usize * u8
    offset_len: usize,
    offset_cap: usize,
    raw: *mut u8,
    raw_cap: usize,
}

impl MixedVec {
    pub fn new() -> Self {
        unsafe { mixedvec_empty() }
    }

    pub fn push(&mut self, ty: PriTy, value: u64) {
        unsafe {
            mixedvec_push(self as *mut Self, ty.to_u8().unwrap(), value);
        }
    }

    pub fn get(&self, index: usize) -> *const u8 {
        unsafe { mixedvec_get(self as *const Self, index) }
    }

    pub fn len(&self) -> usize {
        self.offset_len
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = *const u8> + 'a> {
        let mut i = 0;
        let mixp = self as *const Self;

        box std::iter::from_fn(move || {
            if i >= self.len() {
                None
            } else {
                let ret = unsafe { mixedvec_get(mixp, i) };
                i += 1;
                Some(ret)
            }
        })
    }
}




pub unsafe extern "C" fn mixedvec_empty() -> MixedVec {
    MixedVec {
        offset: null_mut(),
        offset_len: 0, // usize unit len
        offset_cap: 0, // usize unit cap
        raw: null_mut(),
        raw_cap: 0,
    }
}

pub unsafe extern "C" fn mixedvec_push(
    mixp: *mut MixedVec,
    tyid: u8,
    value: u64,
) -> i32 {
    let mix = &mut *mixp;

    let ty;
    if let Some(_ty) = PriTy::from_u8(tyid) {
        ty = _ty
    } else {
        return -1;
    };

    let init_cap = 4;

    if mix.offset_cap == 0 {
        let offset = allocate(size_of::<usize>() * init_cap) as *mut usize;
        *offset = 0;

        mix.offset = offset;
        mix.offset_len = 1;
        mix.offset_cap = init_cap;
    }

    if mix.offset_len == mix.offset_cap {
        let offset_cap = mix.offset_cap * 2;
        let offset = allocate(size_of::<usize>() * offset_cap) as *mut usize;

        copy_nonoverlapping(mix.offset, offset, mix.offset_len);
        deallocate(mix.offset as *mut u8, mix.offset_cap * size_of::<usize>());

        mix.offset = offset;
        mix.offset_cap = offset_cap;
    }

    let last_offset = *mix.offset.add(mix.offset_len - 1);
    let end = last_offset + ty.size_of() as usize;
    *mix.offset.add(mix.offset_len) = end;

    if mix.raw.is_null() {
        mix.raw_cap = 8 * init_cap;
        mix.raw = allocate(mix.raw_cap);
    }

    if mix.raw_cap < end {
        let raw_cap = mix.raw_cap * 2;
        let raw = allocate(raw_cap);

        copy_nonoverlapping(mix.raw, raw, mix.raw_cap);
        deallocate(mix.raw, mix.raw_cap);

        mix.raw = raw;
        mix.raw_cap = raw_cap;
    }

    let p = mix.raw.add(last_offset);

    match ty.size_of() {
        1 => {
            *(p as *mut u8) = value as u8;
        }
        2 => {
            *(p as *mut u16) = value as u16;
        }
        4 => {
            *(p as *mut u32) = value as u32;
        }
        8 => {
            *(p as *mut u64) = value;
        }
        _ => unimplemented!(),
    }

    mix.offset_len += 1;


    0
}


pub unsafe extern "C" fn mixedvec_get(
    mixp: *const MixedVec,
    idx: usize,
) -> *const u8 {
    let mix = &*mixp;

    let offset = *mix.offset.add(idx);
    mix.raw.add(offset)
}



////////////////////////////////////////////////////////////////////////////////
//// N-dimensional space

/// ```no_run
/// raw: [base_tyid(u8), dlen(usize), meta(usize(n)), data(bytes)]
///
/// data: ([key,idx(+1)] [value])...
/// ```
#[derive(Clone)]
#[repr(C)]
pub struct NDS {
    raw: *mut u8,
}

impl NDS {
    pub fn new(base: PriTy, dtys: &[(PriTy, usize)]) -> Self {
        unsafe {
            let dtyids_len = dtys.len();

            let mut dtyids = Array::new(dtyids_len);
            let mut dtylens = Array::new(dtyids_len);

            for (i, (ty, len)) in dtys.iter().enumerate() {
                dtyids[i] = ty.to_u8().unwrap();
                dtylens[i] = *len;
            }

            let raw = nds_create(
                base.to_u8().unwrap(),
                dtyids.as_ptr(),
                dtylens.as_ptr(),
                dtyids_len,
            );

            Self { raw }
        }
    }

    #[allow(unused)]
    fn get_len(&self) -> usize {
        // bytes
        unsafe {
            nds_len(self.raw)
        }
    }

    // fn get(&self, index: &[usize]) -> Option<usize> {
    //     unsafe {
    //         let res = nds_get(self.raw, index.as_ptr()) as *const usize;

    //         if res.is_null() {
    //             None
    //         } else {
    //             Some(*res)
    //         }
    //     }
    // }
}

impl Index<&[usize]> for NDS {
    type Output = usize;

    fn index(&self, index: &[usize]) -> &Self::Output {
        unsafe { &*(nds_get(self.raw, index.as_ptr()) as *const usize) }
    }
}


impl IndexMut<&[usize]> for NDS {
    fn index_mut(&mut self, index: &[usize]) -> &mut Self::Output {
        unsafe { &mut *nds_assoc(self.raw, index.as_ptr()) }
    }
}


impl Drop for NDS {
    fn drop(&mut self) {
        unsafe { nds_deallocate(self.raw) }
    }
}


// 8bit (it seems that 1 or 2 bit is ok!)
fn inject_ty_into_cap(ty: u8, len: usize) -> usize {
    let rem_bytes = size_of::<usize>() - 1;
    let rem_bits = rem_bytes * 8;
    let len_limit = 1usize << rem_bits;

    assert!(len < len_limit);

    len | ((ty as usize) << rem_bits)
}


fn extract_ty_from_cap(packed_len: usize) -> (u8, usize) {
    let rem_bytes = size_of::<usize>() - 1;
    let rem_bits = rem_bytes * 8;

    let ty = ((packed_len & (0xFFusize << rem_bits)) >> rem_bits) as u8;
    let origin_cap = packed_len & !(0xFFusize << rem_bits);

    (ty, origin_cap)
}


const PRI_SIZE: usize = size_of::<usize>();

#[no_mangle]
pub unsafe extern "C" fn nds_create(
    base_tyid: u8,
    dtyids: *const u8,
    dtycaps: *const usize,
    dtyids_len: usize,
) -> *mut u8 {
    let dtyids = from_raw_parts(dtyids, dtyids_len);
    let dtylens = from_raw_parts(dtycaps, dtyids_len);

    // Integrate
    let mut meta = VecDeque::new();
    for (dtyid, dtylen) in zip(dtyids, dtylens) {
        meta.push_front(inject_ty_into_cap(*dtyid, *dtylen));
    }
    let meta_size = size_of::<usize>() * dtyids_len;

    // Calculate data part len
    let data_size = dtylens.iter().fold(PRI_SIZE, |acc, x| {
        acc * x + (PRI_SIZE + size_of::<usize>()) * x
    });

    let raw =  // meta_len, meta(packed dtycaps), base_tyid, data((key, idx)..., value...)
        allocate(size_of::<usize>() + meta_size + 1 + data_size);

    *(raw as *mut usize) = dtyids_len;

    let mut meta_p = (raw as *mut usize).add(1);
    for meta_unit in meta {
        (*meta_p) = meta_unit;
        meta_p = meta_p.add(1);
    }

    *(meta_p as *mut u8) = base_tyid;
    // Box::into_raw(box NDS { raw })
    raw
}


#[no_mangle]
pub unsafe extern "C" fn nds_len(raw: *mut u8) -> usize {
    let meta_len = *(raw as *const usize);

    let meta_p = (raw as *const usize).add(1);
    let meta = from_raw_parts(meta_p, meta_len);

    let meta_plain = parse_nds_meta(meta);

    let (offset, _, len) = meta_plain[0];

    (meta_len + 1) * size_of::<usize>() + 1 + offset * len
}


#[no_mangle]
pub unsafe extern "C" fn nds_deallocate(
    raw: *mut u8
) {
    let len = nds_len(raw);
    deallocate(raw, len);
}


/// (offset, PriTy, len)
fn parse_nds_meta(meta: &[usize]) -> VecDeque<(usize, PriTy, usize)> {
    let mut meta_plain = VecDeque::new();
    let mut offset = PRI_SIZE;

    for packed_len in meta {
        let (tyid, cap) = extract_ty_from_cap(*packed_len);
        let ty = PriTy::from_u8(tyid).unwrap();

        meta_plain.push_front((offset, ty, cap));
        offset = (offset + PRI_SIZE + size_of::<usize>()) * cap;
    }

    meta_plain
}

/// Binary search filledlen
unsafe fn bfindlen(keypair_start: *const u8, cap: usize) -> usize {
    let getkey = |p: *const u8| *(p as *const usize);
    let keypair_unit_size = size_of::<usize>() * 2;

    // find last filled index
    let mut l = 0;
    let mut h = cap; // [l, h)
    let mut fill_len = None;

    while l < h && h > 0 {
        let pivot = (h + l) / 2;

        if getkey(keypair_start.add(pivot * keypair_unit_size)) == 0 {
            if pivot > 0
                && getkey(keypair_start.add((pivot - 1) * keypair_unit_size))
                    > 0
            {
                // BINGO!
                fill_len = Some(pivot);
                break;
            }

            h = pivot;
        } else {
            l = pivot + 1;
        }
    }

    if h == 0 {
        0
    } else if l == cap {
        cap
    } else {
        fill_len.unwrap()
    }
}

unsafe fn bfindkey(
    keypair_start: *const u8,
    attr: usize,
    fill_len: usize,
    cmp: &Box<dyn Fn(usize, usize) -> Ordering>,
) -> *const u8 {
    let getkey = |p: *const u8| *(p as *const usize);
    let keypair_unit_size = size_of::<usize>() * 2;

    let res;
    let mut h = fill_len; // [l, h)
    let mut l = 0;

    let mut pivot = (h + l) / 2;

    loop {
        if l >= h {
            res = Some(keypair_start.add(pivot * keypair_unit_size));
            break;
        }

        pivot = (h + l) / 2;

        let origin_key =
            getkey(keypair_start.add(pivot * keypair_unit_size)) - 1;

        match cmp(attr - 1, origin_key) {
            Ordering::Less => {
                h = pivot;
            }
            Ordering::Equal => {
                // BINGO!
                res = Some(keypair_start.add(pivot * keypair_unit_size));
                break;
            }
            Ordering::Greater => {
                l = pivot + 1;
            }
        }
    }

    res.unwrap()
}


#[no_mangle]
pub unsafe extern "C" fn nds_assoc(
    raw: *mut u8,
    attrs: *const usize,
) -> *mut usize {
    // unpack raw
    let meta_len = *(raw as *const usize);
    let attrs = from_raw_parts(attrs, meta_len);

    let meta_p = (raw as *const usize).add(1);
    let meta = from_raw_parts(meta_p, meta_len);

    let base_ty_p = meta_p.add(meta_len) as *const u8;

    let data = base_ty_p.add(1);

    let mut p = data;

    let meta_plain = parse_nds_meta(meta);

    let getkey = |p: *const u8| *(p as *const usize);
    let getidx = |p: *const u8| *(p as *const usize).add(1);
    let set_keypair = |p: *const u8, key: usize, idx: usize| {
        *(p as *mut usize) = key;
        *(p as *mut usize).add(1) = idx;
    };

    for (attr, (offset, ty, cap)) in zip(attrs, meta_plain) {
        let attr = attr + 1; // To distinguish with unfilled area (0).

        let key_size = PRI_SIZE;
        let idx_size = size_of::<usize>();
        let keypair_unit_size = key_size + idx_size;

        let keypair_start = p;
        let keypair_end = keypair_start.add(keypair_unit_size * cap);
        let value_start = keypair_end;

        // println!(
        //     "assoc keypair_start_addr: {:02x}: {}",
        //     keypair_start as usize,
        //     getkey(keypair_start)
        // );

        // find filled end
        let fill_len = bfindlen(keypair_start, cap);

        // Insert sort

        // bin find
        let cmp = ty.get_cmp();

        if fill_len > 0 {
            let res = bfindkey(keypair_start, attr, fill_len, &cmp);

            // The key doesn't exist yet !
            let mut need_insert_key = true;
            let mut insert_point = res as *mut u8;

            match cmp(attr - 1, getkey(res) - 1) {
                Ordering::Less => {
                }
                Ordering::Equal => {
                    need_insert_key = false;
                }
                Ordering::Greater => {
                    insert_point = (res as *mut u8).add(keypair_unit_size);
                }
            }

            if need_insert_key {
                debug_assert!(fill_len < cap);
                let insert_idx = (insert_point as usize
                    - keypair_start as usize)
                    / keypair_unit_size;

                if insert_idx < fill_len {
                    // delay copy
                    copy(
                        insert_point,
                        insert_point.add(keypair_unit_size),
                        (fill_len - insert_idx) * keypair_unit_size,
                    );
                }

                set_keypair(insert_point, attr, fill_len);
            }

            let valueidx = getidx(insert_point);

            p = value_start.add(valueidx * offset);
        } else {
            set_keypair(keypair_start, attr, 0);

            p = value_start;
        }

        // // println!("off: {}", offset);
        // println!("assoc: {}, filllen: {}", attr - 1, fill_len + 1);
        // for i in (0..fill_len + 1) {
        //     let key = getkey(keypair_start.add(i * keypair_unit_size)) - 1;
        //     let idx = getidx(keypair_start.add(i * keypair_unit_size));
        //     print!("{}({}: {}), ",
        //         key,
        //         idx,
        //         *(value_start.add(idx * offset) as *const usize)
        //     );
        // }
        // println!();
        // println!("value: ");
        // for i in (0..fill_len + 1) {
        //     print!("{}, ", *(value_start.add(i * offset) as *const usize));
        // }
        // println!("\n");
    }


    // update value
    p as *mut usize
}


#[no_mangle]
pub unsafe extern "C" fn nds_get(
    raw: *mut u8,
    attrs: *const usize,
) -> *const usize {
    // unpack raw
    let meta_len = *(raw as *const usize);
    let attrs = from_raw_parts(attrs, meta_len);

    let meta_p = (raw as *const usize).add(1);
    let meta = from_raw_parts(meta_p, meta_len);

    let base_ty_p = meta_p.add(meta_len) as *const u8;

    let data = base_ty_p.add(1);

    let mut p = data;

    let meta_plain = parse_nds_meta(meta);

    let getkey = |p: *const u8| *(p as *const usize);
    let getidx = |p: *const u8| *(p as *const usize).add(1);

    for (attr, (offset, ty, cap)) in zip(attrs, meta_plain) {
        let attr = attr + 1; // To distinguish with unfilled area (0).

        let key_size = PRI_SIZE;
        let idx_size = size_of::<usize>();
        let keypair_unit_size = key_size + idx_size;

        let keypair_start = p;
        let keypair_end = keypair_start.add(keypair_unit_size * cap);

        // println!(
        //     "get keypair_start_addr: {:02x}: {}",
        //     keypair_start as usize,
        //     getkey(keypair_start)
        // );

        // find filled end
        let fill_len = bfindlen(keypair_start, cap);

        if fill_len == 0 {
            return null();
        }

        // Insert sort

        // bin find
        let cmp = ty.get_cmp();

        let res = bfindkey(keypair_start, attr, fill_len, &cmp);

        // The key doesn't exist yet !
        if cmp(attr - 1, getkey(res) - 1) != Ordering::Equal {
            return null();
        }

        let valueidx = getidx(res);
        let value_start = keypair_end;

        p = value_start.add(valueidx * offset)
    }

    p as *const usize
}



#[cfg(test)]
mod tests {
    use std::ffi::CString;

    use super::MixedVec;
    use super::PriTy;
    use super::NDS;
    use crate::aux::{cstr_p, cstr_u};


    #[test]
    fn test_mixedvec() {
        let mut mix = MixedVec::new();

        mix.push(PriTy::Int, 23);
        mix.push(PriTy::Int, 12.111f32.to_bits() as u64);
        mix.push(PriTy::Int, 0xFF);

        unsafe {
            assert_eq!(*(mix.get(0) as *const usize), 23);
            assert_eq!(f32::from_bits(*(mix.get(1) as *const u32)), 12.111);
            assert_eq!(*(mix.get(2) as *const u8), 0xFF);
        }
    }

    #[test]
    fn test_etc() {
        unsafe {
            let p1_0 = cstr_p("aaa");
            let p1_1 = cstr_p("aaa");

            println!(
                "{:#?}",
                CString::from_raw(p1_0).cmp(&CString::from_raw(p1_1))
            );
        }
    }

    #[test]
    fn test_nds() {
        // N1 Int
        let mut n1 = NDS::new(PriTy::Int, &[(PriTy::Int, 4)]);

        n1[&[2]] = 4;
        n1[&[1]] = 1;
        n1[&[3]] = 9;
        n1[&[0]] = 100;

        assert_eq!(n1.get(&[1]).unwrap(), 1);
        assert_eq!(n1.get(&[3]).unwrap(), 9);
        assert_eq!(n1.get(&[2]).unwrap(), 4);

        n1[&[2]] = 6;
        assert_eq!(n1[&[2]], 6);

        // N2 Str
        let mut n2 = NDS::new(PriTy::Int, &[(PriTy::Str, 5)]);

        n2[&[cstr_u("aaa")]] = 123;
        n2[&[cstr_u("bbb")]] = 321;

        assert_eq!(n2[&[cstr_u("bbb")]], 321);
        assert_eq!(n2[&[cstr_u("aaa")]], 123);

        // N3 Nested Int
        let mut n3 = NDS::new(
            PriTy::Int,
            &[
                (PriTy::Int, 5),
                (PriTy::Int, 3),
                (PriTy::Int, 6),
                ]);

        n3[&[2, 1, 4]] = 7;
        n3[&[1, 2, 2]] = 5;
        n3[&[0, 3, 3]] = 6;

        assert_eq!(n3.get(&[2, 1, 4]).unwrap(), 7);
        assert_eq!(n3.get(&[1, 2, 2]).unwrap(), 5);
        assert_eq!(n3.get(&[0, 3, 3]).unwrap(), 6);

        // N4 Mixed Int and Str
        let mut n4 = NDS::new(
            PriTy::Int,
            &[
                (PriTy::Str, 5),
                (PriTy::Int, 3),
                (PriTy::Str, 6),
                ]);

        n4[&[cstr_u("abc"), 1, cstr_u("ee")]] = 7;
        n4[&[cstr_u("abc"), 2, cstr_u("ff")]] = 5;
        n4[&[cstr_u("edf"), 3, cstr_u("ee")]] = 6;

        assert_eq!(n4.get(&[cstr_u("abc"), 1, cstr_u("ee")]).unwrap(), 7);
        assert_eq!(n4.get(&[cstr_u("abc"), 2, cstr_u("ff")]).unwrap(), 5);
        assert_eq!(n4.get(&[cstr_u("edf"), 3, cstr_u("ee")]).unwrap(), 6);

    }
}
