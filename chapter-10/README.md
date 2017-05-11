# Chapter 10: Simple Types

An implementation in Rust of the [`simplebool`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#![feature(box_syntax, box_patterns)]
extern crate simplebool;

use simplebool::{type_of, Term, Context, Type};

fn main() {
    let ctx = Context::new();
    let term = Term::App(box Term::Abs("x".into(), Type::Bool, box Term::Var(0)), box Term::If(box Term::True, box Term::True, box Term::False));

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
