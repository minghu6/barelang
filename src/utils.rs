use std::{fmt::{self, Debug}, iter::Rev};
use std::ops::Range;
use std::{
    iter,
    vec
};


#[inline]
pub fn yes<T>(_: T) -> bool {
    true
}

#[inline]
pub fn no<T>(_: T) -> bool {
    false
}

#[inline]
pub fn but_last_n_str(input: &str, n: isize) -> &str {
    let normal_n = if n >= 0 { n as usize } else { (-n) as usize };
    &input[0..input.len() - normal_n]
}


#[inline]
pub fn char_inc(x: &char) -> Option<char> {
    use std::char::from_u32;

    from_u32((*x as u32) + 1)
}

#[inline]
pub fn char_dec(x: &char) -> Option<char> {
    use std::char::from_u32;

    from_u32((*x as u32) - 1)
}

#[inline]
pub fn char_range(lower: char, upper: char) -> Range<char> {
    lower..char_inc(&upper).unwrap()
}


////////////////////////////////////////////////////////////////////////////////
/////// Stack

#[derive(Clone)]
pub struct Stack<T> {
    _value_vec: Vec<T>
}

impl <T> Stack<T> {
    // staic method
    pub fn new() -> Self {
        Self {
            _value_vec: vec![]
        }
    }

    pub fn push(&mut self, item: T) {
        self._value_vec.push(item)
    }

    pub fn pop(&mut self) -> Option<T> {
        self._value_vec.pop()
    }

    pub fn top(&self) -> Option<&T> {
        self._value_vec.last()
    }

    pub fn len(&self) -> usize {
        self._value_vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self._value_vec.len() == 0
    }

    /// FILO
    pub fn stack_iter(&self) -> StackIter<T> {
        StackIter {
            iter: self._value_vec.iter().rev()
        }
    }

    /// FIFO
    pub fn queue_iter(&self) -> QueueIter<T> {
        QueueIter {
            iter: self._value_vec.iter()
        }
    }

    /// This method will move the content of stack
    pub fn extend_stack(&mut self, income_stack: Stack<T>) {
        for item in income_stack.into_iter().rev() {
            self.push(item);
        }
    }
}

impl <T: Clone> Stack<T> {
    /// Same order with iter (rev)
    pub fn to_vec(&self) -> Vec<T> {
        self._value_vec.iter().rev().cloned().collect::<Vec<T>>()
    }
}

impl <T> iter::IntoIterator for Stack<T> {
    type Item = T;
    type IntoIter = Rev<vec::IntoIter<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self._value_vec.into_iter().rev()
    }
}

/// rev order of Vec
impl <T> From<Vec<T>> for Stack<T> {
    fn from(income: Vec<T>) -> Self {
        Self {
            _value_vec: income.into_iter().rev().collect()
        }
    }
}

impl <T> Extend<T> for Stack<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for item in iter {
            self._value_vec.push(item);
        }
    }
}

impl <T: Debug> fmt::Debug for Stack<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() { return Ok(()) }

        let lastpos = self.len() - 1;
        for (i, item) in self._value_vec.iter().rev().enumerate() {
            if i < lastpos {
                write!(f, "{:?} ", item)?
            } else {
                write!(f, "{:?}", item)?
            }
        }

        Ok(())
    }
}

/// StackIter
pub struct StackIter<'a, T> {
    iter: Rev<std::slice::Iter<'a, T>>
}

impl <'a, T> Iterator for StackIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

/// QueueIter
pub struct QueueIter<'a, T> {
    iter: std::slice::Iter<'a, T>
}

impl <'a, T> Iterator for QueueIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

#[macro_export]
macro_rules! stack {
    ( $($value:expr),* ) => {
        {
            let mut _stack = $crate::utils::Stack::new();

            $(
                _stack.push($value);
            )*

            _stack
        }
    };
}


////////////////////////////////////////////////////////////////////////////////
/////// Meta Macro Tools




#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_but_last_n() {
        use super::{but_last_n_str};

        assert_eq!(but_last_n_str("abc_d", -2), "abc");
        assert_eq!(but_last_n_str("abc_d", 2), "abc");
    }

    #[test]
    fn test_stack_struct() {
        use super::Stack;

        let mut stack = Stack::new();
        stack.extend(vec![1, 2, 3].into_iter());

        assert_eq!(stack.to_vec(), vec![3, 2, 1]);
        assert_eq!(stack![1, 2, 3].to_vec(), vec![3, 2, 1]);

        stack.extend_stack(stack![4, 5]);
        assert_eq!(stack.to_vec(), vec![5, 4, 3, 2, 1]);

        println!("{:?}", stack);
    }

    #[test]
    fn test_vec_like_macro_rules() {
        let queue = vecdeq![1, 2];

        println!("{:?}", queue);
    }
}
