## BareLang

舍弃一些东西, 来寻求简洁和某种程度的形式与内在的统一.

## Dependencies

1. LLVM-12

1. Rust nightly (need std)




## Grammar Specification

1. 2元运算符没有重载, 2元运算符中缀表示, 2元运算允许一定规则下的类型自动转换

```
int -> float

u8 -> u32
       | -> usize(64) -> u64 -> i64
       | -> usize(32) -> i32 -> usize(64)
```


2. 强调纯函数操作，

 方便注册 对称性(symmetry):

 ```
       symmetry(fun-prefix, left | right)

 ```

