#![warn(missing_docs)]
#![feature(box_syntax, box_patterns)]

//! A Rust implementation of chapter 4 of Benjamin C. Pierce's "Types and Programming Languages"
//! `arith` language.
//!
//! c.f. https://www.cis.upenn.edu/~bcpierce/tapl/checkers/arith/core.ml for the sample OCaml
//! implementation.
//!
//! # Example
//!
//! This library exposes an `eval` function which takes any `Term` and repeatedly evaluates it
//! according to our evaluation rules until no rule applies.
//!
//! ```rust
//! #![feature(box_syntax)]
//! use arith::eval;
//! use arith::Term::{Succ, Pred, Zero, IsZero, True};
//!
//! let succ_zero = Succ(box Zero);
//! let pred_succ_zero = Pred(box succ_zero);
//! let iszero_pred_succ_zero = IsZero(box pred_succ_zero);
//!
//! assert_eq!(True, eval(&iszero_pred_succ_zero));
//! ```

pub use syntax::Term;
pub use evaluation::eval;
pub use grammar::parse_Term as parse;

mod syntax;
mod evaluation;
mod grammar;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arith() {
        assert_eq!(Ok(Term::True), parse("true"));
        assert_eq!(Ok(Term::False), parse("false"));
        assert_eq!(Ok(Term::Zero), parse("0"));
        assert_eq!(Ok(Term::Pred(Box::new(Term::Zero))), parse("pred 0"));
        assert_eq!(Ok(Term::Succ(Box::new(Term::Zero))), parse("succ 0"));
        assert_eq!(Ok(Term::IsZero(Box::new(Term::Zero))), parse("iszero 0"));
        assert_eq!(Ok(Term::If(Box::new(Term::True),
                               Box::new(Term::Zero),
                               Box::new(Term::Succ(Box::new(Term::Zero))))),
                   parse("if true then 0 else succ 0"));
        assert_eq!(Ok(Term::Succ(Box::new(Term::Pred(Box::new(Term::Zero))))),
                   parse("succ pred 0"));
    }
}
