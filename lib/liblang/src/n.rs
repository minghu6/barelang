use std::{
    collections::VecDeque,
    iter::zip,
    mem::size_of,
    ops::Add,
    ptr::{copy_nonoverlapping, null_mut},
    slice::from_raw_parts,
};

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::allocator::{allocate, deallocate};


#[derive(FromPrimitive, ToPrimitive)]
pub enum PriTy {
    U8 = 0,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    USIZE,

    F32,
    F64,
    // Ptr,  // usize
}

impl PriTy {
    fn size_of(&self) -> usize {
        match self {
            PriTy::U8 => 1,
            PriTy::I8 => 1,
            PriTy::U16 => 2,
            PriTy::I16 => 2,
            PriTy::U32 => 4,
            PriTy::I32 => 4,
            PriTy::U64 => 8,
            PriTy::I64 => 8,
            PriTy::USIZE => 8, // en... FIXME:!
            PriTy::F32 => 4,
            PriTy::F64 => 8,
            PriTy::Str => 8,
        }
    }

    fn is_valid_vector_index(&self) -> bool {
        match &self {
            #[cfg(target_pointer_width = "64")]
            PriTy::U8 | PriTy::U16 | PriTy::U32 | PriTy::U64 => true,

            #[cfg(target_pointer_width = "32")]
            PriTy::U8 | PriTy::U16 | PriTy::U32 => true,

            #[cfg(target_pointer_width = "16")]
            PriTy::U8 | PriTy::U16 => true,

            _ => false,
        }
    }
}


// trait MapKey = Hash + Eq;

// enum Map<K: MapKey, V> {
//     Vec(Vec<V>),
//     IndexMap(IndexMap<K, V>)
// }

// impl<K: MapKey, V> Map<K, V> {
//     fn insert(&mut self) {
//         match self {
//             Map::Vec(mut vec) => vec.pu,
//             Map::IndexMap(_) => todo!(),
//         }
//     }

// }


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
        let mixp = unsafe { self as *const Self };

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

#[derive(Clone, Copy)]
#[repr(C)]
pub struct NDS {
    raw: *mut u8, // [base_tyid(u8), dlen(usize), meta(usize(n)), data(bytes)]
}


fn inject_ty_into_len(ty: u8, len: usize) -> usize {
    let rem_bytes = size_of::<usize>() - 1;
    let len_limit = 1usize << (rem_bytes * 8);

    assert!(len < len_limit);

    len | ((ty as usize) << (rem_bytes * 8))
}

fn extract_ty_from_len(packed_len: usize) -> (u8, usize) {
    let rem_bytes = size_of::<usize>() - 1;

    let ty = (packed_len & (0xFF << (rem_bytes * 8))) as u8;
    let origin_len = packed_len & !(0xFF << (rem_bytes * 8));

    (ty, origin_len)
}

fn parse_nds_meta(base_ty: PriTy, meta: &[usize]) -> Vec<usize> {
    let mut offset = base_ty.size_of();
    let mut offsets = VecDeque::new();

    for packed_len in meta {
        let (tyid, len) = extract_ty_from_len(*packed_len);

        let mut unit = offset;

        if !ty.is_valid_vector_index() {
            unit += ty.size_of();
        }

        offsets.push_front(unit);
        offset = unit * len;
    }

    offsets
}


pub unsafe extern "C" fn nds_create(
    base_ty: u8,
    meta_dty: *const u8,
    meta_dlen: *const usize, // size_t
    meta_len: usize,
) -> NDS {
    let dtyids = from_raw_parts(meta_dty, meta_len);
    let dlens = from_raw_parts(meta_dlen, meta_len);

    let meta_len_part_size = size_of::<usize>();
    let meta_part_size = size_of::<usize>() * meta_len;

    let mut data_part_size = PriTy::from_u8(base_ty).unwrap().size_of();
    let mut meta_part = VecDeque::new();

    for (dty, dlen) in zip(dtyids, dlens).rev() {
        let ty = PriTy::from_u8(*dty).unwrap();

        if ty.is_valid_vector_index() {
            data_part_size *= dlen;
        } else {
            data_part_size = (data_part_size + ty.size_of()) * dlen;
        }

        meta_part.push_front(inject_ty_into_len(*dty, *dlen));
    }

    let raw =
        allocate(1 + meta_len_part_size + meta_part_size + data_part_size);

    *raw = base_ty;

    let mut p = raw.add(1) as *mut usize;
    (*p) = meta_len_part_size;

    for meta_unit in meta_part {
        p = p.add(1);
        (*p) = meta_unit;
    }

    NDS { raw }
}


unsafe fn nds_assoc(nds: *mut NDS, attrs: *const MixedVec, value: u64) -> i32 {
    let attrs = &*attrs;
    let raw = (*nds).raw;

    let base_tyid = *raw;
    let base_ty = PriTy::from_u8(base_tyid).unwrap();

    let dlen = *(raw.add(1) as *const usize);
    let meta = (raw as *const usize).add(1);
    let meta_s = from_raw_parts(meta, dlen);
    let data = meta.add(dlen) as *mut u8;

    let mut p = data;

    let offsets = parse_nds_meta(base_ty, meta_s);

    for (offset, (attr_p, packed_len)) in
        zip(offsets, zip(attrs.iter(), meta_s))
    {
        let (tyid, len) = extract_ty_from_len(*packed_len);
        let ty: PriTy = PriTy::from_u8(tyid).unwrap();

        match ty {
            PriTy::U8 => {
                let idx = *attr_p;

                p = p.add(idx * offset);
            },
            PriTy::I8 => {

            }
            PriTy::U16 => {
                let idx = *attr_p as *const u16;

                p = p.add(idx * offset);
            }
            PriTy::I16 => {}
            PriTy::U32 => {
                let idx = *attr_p as *const u32;

                p = p.add(idx * offset);
            }
            PriTy::I32 => {}
            PriTy::U64 => {
                let idx = *attr_p as *const u64;

                p = p.add(idx * offset);
            }
            PriTy::I64 => {}
            PriTy::USIZE => {
                let idx = *attr_p as *const usize;

                p = p.add(idx * offset);
            }

            PriTy::F32 => {

            }
            PriTy::F64 => {

            }
        }
    }

    0
}



#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::MixedVec;
    use super::PriTy;


    #[test]
    fn test_mixedvec() {
        let mut mix = MixedVec::new();

        mix.push(PriTy::USIZE, 23);
        mix.push(PriTy::F32, 12.111f32.to_bits() as u64);
        mix.push(PriTy::I8, 0xFF);

        unsafe {
            assert_eq!(*(mix.get(0) as *const usize), 23);
            assert_eq!(f32::from_bits(*(mix.get(1) as *const u32)), 12.111);
            assert_eq!(*(mix.get(2) as *const u8), 0xFF);
        }
    }

}
