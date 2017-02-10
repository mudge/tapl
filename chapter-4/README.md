# Chapter 4: Arithmetic Expressions

An implementation in Rust of the [`arith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith/core.ml) syntax and evaluation rules.

```rust
use arith::eval;
use arith::Term::{Succ, Pred, Zero, IsZero, True};

let succ_zero = Succ(Box::new(Zero));
let pred_succ_zero = Pred(Box::new(succ_zero));
let iszero_pred_succ_zero = IsZero(Box::new(pred_succ_zero));

assert_eq!(True, eval(iszero_pred_succ_zero));
```
