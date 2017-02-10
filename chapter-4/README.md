# Chapter 4: Arithmetic Expressions

An implementation in Rust of the [`arith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith/core.ml) syntax and evaluation rules.

```rust
extern crate arith;

use arith::eval;
use arith::Term::*;

fn main() {
    let program = If(Box::new(IsZero(Box::new(Pred(Box::new(Succ(Box::new(Zero))))))),
                     Box::new(Succ(Box::new(Succ(Box::new(Pred(Box::new(Zero))))))),
                     Box::new(False));

    println!("Source program:    {}", program);
    println!("Evaluated program: {}", eval(program));
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ 0 else false
Evaluated program: succ succ 0
```
