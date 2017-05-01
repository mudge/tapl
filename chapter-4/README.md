# Chapter 4: Arithmetic Expressions

An implementation in Rust of the [`arith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#[macro_use(arith)]
extern crate arith;

use arith::{eval, Term};

fn main() {
    let program = arith! { if (iszero pred succ 0) then (succ succ pred 0) else (false) };

    println!("Source program:    {}", program);
    println!("Evaluated program: {}", eval(&program));
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ pred 0 else false
Evaluated program: succ succ 0
```
