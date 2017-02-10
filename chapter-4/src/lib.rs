#![warn(missing_docs)]

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
//! use arith::eval;
//! use arith::Term::{Succ, Pred, Zero, IsZero, True};
//!
//! let succ_zero = Succ(Box::new(Zero));
//! let pred_succ_zero = Pred(Box::new(succ_zero));
//! let iszero_pred_succ_zero = IsZero(Box::new(pred_succ_zero));
//!
//! assert_eq!(True, eval(iszero_pred_succ_zero));
//! ```

use std::error;
use std::fmt;
use std::result;

pub use syntax::Term;
pub use evaluation::eval;

mod syntax;
mod evaluation;

/// A type alias for `Result` so we can fix the error type to `Error`.
pub type Result<T> = result::Result<T, Error>;

/// The possible errors raised during evaluation.
#[derive(PartialEq, Debug)]
pub enum Error {
    /// An error when no evaluation rule exists for the given `Term`.
    NoRuleApplies(Term),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::NoRuleApplies(_) => "no evaluation rule applies",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::NoRuleApplies(ref term) => write!(f, "no evaluation applies for term {}", term),
        }
    }
}
