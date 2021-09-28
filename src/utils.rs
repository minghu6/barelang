use std::path::PathBuf;


pub type CounterType = impl FnMut() -> usize;

pub fn gen_counter() -> CounterType {
    gen_counter_1(0)
}

pub fn gen_counter_1(init: usize) -> CounterType {
    let mut count = init;

    move || {
        let old_count = count;
        count += 1;
        old_count
    }
}


#[derive(Debug, PartialEq, Eq)]
pub enum PrintTy {
    StdErr,
    File(PathBuf)
}

impl PrintTy {
    pub fn get_path(&self) -> Option<PathBuf> {
        if let Self::File(ref path) = self {
            Some(path.clone())
        }
        else {
            None
        }
    }
}




#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_vec_like_macro_rules() {
        let queue = vecdeq![1, 2];

        println!("{:?}", queue);
    }
}
