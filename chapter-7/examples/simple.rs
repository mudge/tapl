#[macro_use(untyped)]
extern crate untyped;

use untyped::{eval, Term};

fn main() {
    let term = untyped! { ((λ x . 0) ((λ x . 0) (λ z . ((λ x . 0) 0)))) };
    let evaluated_term = eval(&term);

    println!("Source term:           {}", term);
    println!("Evaluated term:        {}", evaluated_term);
}
