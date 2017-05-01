# Chapter 8: Typed Arithmetic Expressions

An implementation in Rust of the [`tyarith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/tyarith/core.ml) syntax, evaluation and typing rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#[macro_use(tyarith)]
extern crate tyarith;

use tyarith::{type_of, Term};

fn main() {
    let program = tyarith! { if (iszero pred succ 0) then (succ succ pred true) else (0) };

    println!("Source program:    {}", program);
    match type_of(&program) {
        Ok(t) => println!("Type of program:   {}", t),
        Err(error) => println!("Type error:        {}", error),
    }
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ pred true else 0
Type error:        pred with a non-numeric argument
```
