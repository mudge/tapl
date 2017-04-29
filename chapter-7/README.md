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
        box Abs(
            box Abs(
                box App(
                    box Var(1),
                    box Var(0)
                )
            )
        ),
        box Abs(
            box Var(2)
        )
    );

    println!("Source term:    {}", term);
    println!("Evaluated term: {}", eval(&term));
}
```

Produces:

```
Source term:    ((λ. (λ. (1 0))) (λ. 2))
Evaluated term: (λ. ((λ. 3) 0))
```
