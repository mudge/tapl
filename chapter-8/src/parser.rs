//! A parser for the arith language.
//!
//! t :=
//!      true
//!      false
//!      if t then t else t
//!      0
//!      succ t
//!      pred t
//!      iszero t

use std::error;
use std::fmt;

use Term;

/// Errors that may occur during parsing.
#[derive(PartialEq, Debug, Clone)]
pub enum Error {
    /// An if term without a consequent
    IfWithoutConsequent,
    /// An if term without an alternative
    IfWithoutAlternative,
    /// An unknown token
    UnknownToken(String),
    /// When the end of the input is encountered unexpected
    UnexpectedEndOfInput,
    /// When another token is found instead of the end of input
    ExpectedEndOfInput(String),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IfWithoutConsequent => "if statement without consequent",
            Error::IfWithoutAlternative => "if statement without alternative",
            Error::UnknownToken(_) => "unknown token",
            Error::UnexpectedEndOfInput => "unexpected end of input",
            Error::ExpectedEndOfInput(_) => "expected end of input, found another token",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::IfWithoutConsequent => write!(f, "if statement without consequent"),
            Error::IfWithoutAlternative => write!(f, "if statement without alternative"),
            Error::UnknownToken(ref unknown) => write!(f, "unknown token {}", unknown),
            Error::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            Error::ExpectedEndOfInput(ref unknown) => write!(f, "expected end of input, found {}", unknown),
        }
    }
}

/// Parse a string term of the arith language into `Term`s.
///
/// # Example
///
/// ```rust
/// #![feature(box_syntax)]
/// use tyarith::{parse, Term};
///
/// let term = parse("if true then true else false");
/// assert_eq!(Ok(Term::If(box Term::True, box Term::True, box Term::False)), term);
/// ```
pub fn parse(input: &str) -> Result<Term, Error> {
    let mut tokens = input.split_whitespace();
    let term = parse_t(&mut tokens);

    match tokens.next() {
        Some(unexpected) => Err(Error::ExpectedEndOfInput(unexpected.into())),
        None => term,
    }
}

fn parse_t(mut tokens: &mut Iterator<Item=&str>) -> Result<Term, Error> {
    match tokens.next() {
        Some("true") => Ok(Term::True),
        Some("false") => Ok(Term::False),
        Some("0") => Ok(Term::Zero),
        Some("succ") => parse_t(&mut tokens).map(|t| Term::Succ(box t)),
        Some("pred") => parse_t(&mut tokens).map(|t| Term::Pred(box t)),
        Some("iszero") => parse_t(&mut tokens).map(|t| Term::IsZero(box t)),
        Some("if") => {
            let guard = parse_t(&mut tokens)?;

            match tokens.next() {
                Some("then") => {
                    let consequent = parse_t(&mut tokens)?;

                    match tokens.next() {
                        Some("else") => {
                            let alternate = parse_t(&mut tokens)?;

                            Ok(Term::If(box guard, box consequent, box alternate))
                        }
                        _ => Err(Error::IfWithoutAlternative),
                    }
                }
                _ => Err(Error::IfWithoutConsequent),
            }
        }
        Some(unknown) => Err(Error::UnknownToken(unknown.into())),
        None => Err(Error::UnexpectedEndOfInput),
    }
}
