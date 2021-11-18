

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

