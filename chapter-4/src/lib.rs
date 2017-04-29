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
//! #![feature(box_syntax, box_patterns)]
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

mod syntax;
mod evaluation;
