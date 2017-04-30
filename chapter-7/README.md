# Chapter 7: The Untyped Lambda-Calculus

An implementation in Rust of the [`untyped`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#![feature(box_syntax, box_patterns)]
extern crate untyped;

use untyped::{eval, print_term};
use untyped::Term::*;

fn main() {
    let ctx = Vec::new();
    let term = App(
        box Abs("x".to_owned(), box Var(0)),
        box App(
            box Abs("x".to_owned(), box Var(0)),
            box Abs(
                "z".to_owned(),
                box App(
                    box Abs("x".to_owned(), box Var(1)),
                    box Var(0)
                )
            )
        )
    );

    println!("Source term:    {}", term);
    println!("Evaluated term: {}", eval(&term));
    println!("Pretty term:    {}", print_term(&ctx, &term));
}
```

Produces:

```
Source term:    ((λ. 0) ((λ. 0) (λ. ((λ. 1) 0))))
Evaluated term: (λ. ((λ. 1) 0))
Pretty term:    ((λx. x) ((λx. x) (λz. ((λx. x) z))))
```
