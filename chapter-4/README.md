# Chapter 4: Arithmetic Expressions

An implementation in Rust of the [`arith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
extern crate arith;

use arith::{parse, eval};

fn main() {
    let program = parse("if iszero pred succ 0 then succ succ pred 0 else false");

    match program {
        Ok(term) => {
            println!("Source program:    {}", term);
            println!("Evaluated program: {}", eval(&term));
        }
        Err(e) => println!("Parse error:       {}", e),
    }
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ pred 0 else false
Evaluated program: succ succ 0
```
