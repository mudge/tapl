# Chapter 10: Simple Types

An implementation in Rust of the [`simplebool`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
extern crate simplebool;

use simplebool::{Context, parse, type_of};

fn main() {
    let ctx = Context::new();
    let term = parse("λx:Bool . 0 if true then true else false").unwrap();

    println!("Source term:    {}", term);
    match type_of(&ctx, &term) {
        Ok(ty) => println!("Resulting type: {}", ty),
        Err(err) => println!("Type error:     {}", err),
    }
}
```

Produces:

```
Source term:    ((λx:Bool. x) if true then true else false)
Resulting type: Bool
```
