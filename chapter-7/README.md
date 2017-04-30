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
        box Abs("x".into(), box Var(0)),
        box App(
            box Abs("x".into(), box Var(0)),
            box Abs(
                "z".into(),
                box App(
                    box Abs("x".into(), box Var(1)),
                    box Var(0)
                )
            )
        )
    );
    let evaluated_term = eval(&term);

    println!("Source term:           {}", term);
    println!("Pretty term:           {}", print_term(&ctx, &term));
    println!("Evaluated term:        {}", evaluated_term);
    println!("Pretty evaluated term: {}", print_term(&ctx, &evaluated_term));
}
```

Produces:

```
Source term:           ((λ. 0) ((λ. 0) (λ. ((λ. 1) 0))))
Pretty term:           ((λx. x) ((λx. x) (λz. ((λx. x) z))))
Evaluated term:        (λ. ((λ. 1) 0))
Pretty evaluated term: (λz. ((λx. x) z))
```
