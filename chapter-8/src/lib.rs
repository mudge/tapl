#![warn(missing_docs)]
#![feature(box_syntax, box_patterns)]

//! A Rust implementation of chapter 8 of Benjamin C. Pierce's "Types and Programming Languages"
//! `arith` language.
//!
//! c.f. https://www.cis.upenn.edu/~bcpierce/tapl/checkers/tyarith/core.ml for the sample OCaml
//! implementation.
//!
//! # Example
//!
//! This library exposes an `eval` function which takes any `Term` and repeatedly evaluates it
//! according to our evaluation rules until no rule applies.
//!
//! ```rust
//! #![feature(box_syntax, box_patterns)]
//! use tyarith::eval;
//! use tyarith::Term::{Succ, Pred, Zero, IsZero, True};
//!
//! let succ_zero = Succ(box Zero);
//! let pred_succ_zero = Pred(box succ_zero);
//! let iszero_pred_succ_zero = IsZero(box pred_succ_zero);
//!
//! assert_eq!(True, eval(iszero_pred_succ_zero));
//! ```

use std::error;
use std::fmt;
use std::result;

pub use syntax::{Term, Type};
pub use evaluation::eval;
pub use typing::type_of;

mod syntax;
mod evaluation;
mod typing;

/// A type alias for convenience so we can fix the error to our own `Error` type.
pub type Result<T> = result::Result<T, Error>;

/// Type errors that may occur.
#[derive(PartialEq, Debug, Clone)]
pub enum Error {
    /// An if term with a non-boolean condition
    NonBoolCondition,
    /// An if term with arms that result in different types
    MismatchingArms,
    /// A succ term with a non-numeric argument
    NonNumericSucc,
    /// A pred term with a non-numeric argument
    NonNumericPred,
    /// A iszero term with a non-numeric argument
    NonNumericIsZero,
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::NonBoolCondition => "if statement with a non-boolean condition",
            Error::MismatchingArms => "if statement with arms resulting in different types",
            Error::NonNumericSucc => "succ with a non-numeric argument",
            Error::NonNumericPred => "pred with a non-numeric argument",
            Error::NonNumericIsZero => "iszero with a non-numeric argument",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::NonBoolCondition => write!(f, "if statement with a non-boolean condition"),
            Error::MismatchingArms => write!(f, "if statement with arms resulting in different types"),
            Error::NonNumericSucc => write!(f, "succ with a non-numeric argument"),
            Error::NonNumericPred => write!(f, "pred with a non-numeric argument"),
            Error::NonNumericIsZero => write!(f, "iszero with a non-numeric argument"),
        }
    }
}
