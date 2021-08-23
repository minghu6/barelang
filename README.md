## Compile


### Lex

#### test

`cargo test -- --nocapture test_lex`


## Source Structure

| id | Source | Output/Function |
| -- | ------ | ------ |
|0| lexer |       `Stack<token>: 同构`|
|1| synax_parser |    `ST: 同构`|
|2| semantic_analyzer | `ML: 异构 \| ML refactor` |
|3| codegen | `LLVM IR` |
|4(0,1)| rule | `Controller of lexer,synax_parser` |
|5(4)| gram | `Engine of rule` |
|6(1,2,3)| badata | `Model of ML` |
|-| dsl | `all declarative macros Service releated` |
|-| rslib | `rust library symbol table` |
|-| main | `interface`|

