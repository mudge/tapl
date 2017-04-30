# Chapter 7: The Untyped Lambda-Calculus

An implementation in Rust of the [`untyped`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```rust
#![feature(box_syntax, box_patterns)]
extern crate untyped;

use untyped::eval;
use untyped::Term::*;

fn main() {
    let term = App(
        box Abs("x".into(), box Var(0)),
        box App(
            box Abs("x".into(), box Var(0)),
            box Abs(
                "z".into(),
                box App(
                    box Abs("x".into(), box Var(0)),
                    box Var(0)
                )
            )
        )
    );
    let evaluated_term = eval(&term);

    println!("Source term:           {}", term);
    println!("Evaluated term:        {}", evaluated_term);
}
```

Produces:

```
Source term:           ((λx. x) ((λx. x) (λz. ((λx. x) z))))
Evaluated term:        (λz. ((λx. x) z))
```
