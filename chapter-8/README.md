# Chapter 8: Typed Arithmetic Expressions

An implementation in Rust of the [`tyarith`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/tyarith/core.ml) syntax, evaluation and typing rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
extern crate tyarith;

use tyarith::{parse, type_of};

fn main() {
    let program = parse("if iszero pred succ 0 then succ succ pred true else 0");

    match program {
        Ok(term) => {
            println!("Source program:    {}", term);

            match type_of(&term) {
                Ok(t) => println!("Type of program:   {}", t),
                Err(error) => println!("Type error:        {}", error),
            }
        }
        Err(e) => println!("Parse error:        {}", e),
    }
}
```

Produces:

```
Source program:    if iszero pred succ 0 then succ succ pred true else 0
Type error:        pred with a non-numeric argument
```
