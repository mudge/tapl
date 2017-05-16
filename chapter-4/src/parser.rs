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
/// use arith::{parse, Term};
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_zero_equals_zero_term() {
        let term = parse("0");

        assert_eq!(Ok(Term::Zero), term);
    }

    #[test]
    fn parse_true_equals_true_term() {
        let term = parse("true");

        assert_eq!(Ok(Term::True), term);
    }

    #[test]
    fn parse_false_equals_false_term() {
        let term = parse("false");

        assert_eq!(Ok(Term::False), term);
    }

    #[test]
    fn parse_succ_equals_succ_term() {
        let term = parse("succ 0");

        assert_eq!(Ok(Term::Succ(box Term::Zero)), term);
    }

    #[test]
    fn parse_nested_succ_equals_succ_term() {
        let term = parse("succ succ 0");

        assert_eq!(Ok(Term::Succ(box Term::Succ(box Term::Zero))), term);
    }

    #[test]
    fn parse_pred_equals_pred_term() {
        let term = parse("pred 0");

        assert_eq!(Ok(Term::Pred(box Term::Zero)), term);
    }

    #[test]
    fn parse_nested_pred_equals_pred_term() {
        let term = parse("pred pred 0");

        assert_eq!(Ok(Term::Pred(box Term::Pred(box Term::Zero))), term);
    }

    #[test]
    fn parse_iszero_equals_iszero_term() {
        let term = parse("iszero 0");

        assert_eq!(Ok(Term::IsZero(box Term::Zero)), term);
    }

    #[test]
    fn parse_nested_iszero_equals_iszero_term() {
        let term = parse("iszero succ 0");

        assert_eq!(Ok(Term::IsZero(box Term::Succ(box Term::Zero))), term);
    }

    #[test]
    fn parse_if_equals_if_term() {
        let term = parse("if true then true else false");

        assert_eq!(Ok(Term::If(box Term::True, box Term::True, box Term::False)),
                   term);
    }

    #[test]
    fn parse_if_with_terms_equals_if_term() {
        let term = parse("if iszero 0 then succ 0 else pred 0");

        assert_eq!(Ok(Term::If(box Term::IsZero(box Term::Zero),
                               box Term::Succ(box Term::Zero),
                               box Term::Pred(box Term::Zero))),
                   term);
    }

    #[test]
    fn parse_nested_if_equals_if_term() {
        let term = parse("if if true then true else false then if true then true else false else \
                          if false then true else false");

        assert_eq!(Ok(Term::If(box Term::If(box Term::True, box Term::True, box Term::False),
                               box Term::If(box Term::True, box Term::True, box Term::False),
                               box Term::If(box Term::False, box Term::True, box Term::False))),
                   term);
    }

    #[test]
    fn parse_if_without_condition_returns_error() {
        let term = parse("if");

        assert_eq!(Err(Error::UnexpectedEndOfInput), term);
    }

    #[test]
    fn parse_if_without_then_returns_error() {
        let term = parse("if true");

        assert_eq!(Err(Error::IfWithoutConsequent), term);
    }

    #[test]
    fn parse_if_without_else_returns_error() {
        let term = parse("if true then true");

        assert_eq!(Err(Error::IfWithoutAlternative), term);
    }

    #[test]
    fn parse_unknown_term_returns_error() {
        let term = parse("foo");

        assert_eq!(Err(Error::UnknownToken("foo".into())), term);
    }

    #[test]
    fn parse_trailing_terms_returns_error() {
        let term = parse("if true then false else true false");

        assert_eq!(Err(Error::ExpectedEndOfInput("false".into())), term);
    }
}
