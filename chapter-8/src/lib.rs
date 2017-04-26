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

pub use syntax::{Term, Type};
pub use evaluation::eval;
pub use typing::type_of;

mod syntax;
mod evaluation;
mod typing;