# Chapter 8: Typed Arithmetic Expressions

An implementation in Rust of the [`tyarith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/tyarith/core.ml) syntax, evaluation and typing rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#![feature(box_syntax, box_patterns)]
extern crate tyarith;

use tyarith::type_of;
use tyarith::Term::*;

fn main() {
    let program = If(box IsZero(box Pred(box Succ(box Zero))),
                     box Succ(box Succ(box Pred(box Zero))),
                     box Zero);

    println!("Source program:    {}", program);
    println!("Type of program:   {}", type_of(program).expect("Does not type-check!"));
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ pred 0 else 0
Type of program:   Nat
```