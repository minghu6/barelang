#![feature(extend_one)]
#![feature(type_alias_impl_trait)]
#![feature(let_chains)]
#![feature(option_get_or_insert_default)]
#![feature(path_file_prefix)]
#![feature(box_syntax)]

#![allow(mixed_script_confusables)]
#![allow(incomplete_features)]

pub mod frontend;
pub mod middleware;
pub mod backend;

pub mod utils;
pub mod error;
pub mod rsclib;
pub mod dbi;



use bacommon::config::VerboseLv;
pub use proc_macros::{
    make_vec_macro_rules,
    make_char_matcher_rules,
    make_token_matcher_rules,
    make_simple_error_rules,
    //ht
};

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);

pub static mut VERBOSE: VerboseLv = VerboseLv::V0;

/// check if current verbose level meet the required verbose lv
#[inline]
pub fn verbose_enable(verbose: VerboseLv) -> bool {
    unsafe {
        VERBOSE >= verbose
    }
}



#[cfg(test)]
mod test {

    #[test]
    fn test_ht() {
        use crate::ht;

        let a = 1;
        let c = 3;
        let b = vec![2];

        println!("{:?}", ht![a | b]);

        println!("{:?}", ht![c | ht![a| ht![a | b]]]);

        println!("{:?}", ht![2|b]);

        println!("{:?}", ht![2| vec![3] ]);

        println!("{:?}", ht![0 | ht![1 | ht![2| ht![3]]]]);

    }
}
