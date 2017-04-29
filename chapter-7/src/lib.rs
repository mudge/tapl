#![warn(missing_docs)]
#![feature(box_syntax, box_patterns)]

//! A Rust implementation of chapter 7 of Benjamin C. Pierce's "Types and Programming Languages"
//! `untyped` language.
//!
//! c.f. https://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped/core.ml for the sample OCaml
//! implementation.

use std::fmt;

/// A term in the untyped Lambda Calculus.
#[derive(PartialEq, Debug, Clone)]
pub enum Term {
    /// Variable.
    Var(i32),
    /// Abstraction.
    Abs(Box<Term>),
    /// Application.
    App(Box<Term>, Box<Term>),
}

/// Evaluate a given `Term` until no more evaluation rules apply.
pub fn eval(t: &Term) -> Term {
    eval1(t).map_or(t.clone(), |t_prime| eval(&t_prime))
}

fn eval1(t: &Term) -> Option<Term> {
    match *t {
        Term::App(box Term::Abs(box ref t12), box ref v2) if is_val(v2) => Some(term_subst_top(v2, t12)),
        Term::App(box ref v1, box ref t2) if is_val(v1) => eval1(t2).map(|t2_prime| Term::App(box v1.clone(), box t2_prime)),
        Term::App(box ref t1, box ref t2) => eval1(t1).map(|t1_prime| Term::App(box t1_prime, box t2.clone())),
        _ => None,
    }
}

fn is_val(t: &Term) -> bool {
    match *t {
        Term::Abs(_) => true,
        _ => false,
    }
}

fn term_subst_top(s: &Term, t: &Term) -> Term {
    term_shift(-1, &term_subst(0, &term_shift(1, s), t))
}

fn term_subst(j: i32, s: &Term, t: &Term) -> Term {
    term_subst_walk(0, t, j, s)
}

fn term_subst_walk(c: i32, t: &Term, j: i32, s: &Term) -> Term {
    match *t {
        Term::Var(x) => {
            if x == j + c {
                term_shift(c, s)
            } else {
                Term::Var(x)
            }
        },
        Term::Abs(ref t1) => Term::Abs(box term_subst_walk(c + 1, t1, j, s)),
        Term::App(ref t1, ref t2) => Term::App(box term_subst_walk(c, t1, j, s), box term_subst_walk(c, t2, j, s)),
    }
}

fn term_shift(d: i32, t: &Term) -> Term {
    term_shift_walk(0, t, d)
}

fn term_shift_walk(c: i32, t: &Term, d: i32) -> Term {
    match *t {
        Term::Var(x) => {
            if x >= c {
                Term::Var(x + d)
            } else {
                Term::Var(x)
            }
        },
        Term::Abs(ref t1) => Term::Abs(box term_shift_walk(c + 1, t1, d)),
        Term::App(ref t1, ref t2) => Term::App(box term_shift_walk(c, t1, d), box term_shift_walk(c, t2, d)),
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(x) => write!(f, "{}", x),
            Term::App(ref t1, ref t2) => write!(f, "({} {})", t1, t2),
            Term::Abs(ref t1) => write!(f, "(λ. {})", t1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn var_is_not_a_val() {
        assert!(!is_val(&Term::Var(0)));
    }

    #[test]
    fn abs_is_a_val() {
        assert!(is_val(&Term::Abs(box Term::Var(0))));
    }

    #[test]
    fn displaying_a_var_returns_its_index() {
        let var = format!("{}", Term::Var(0));

        assert_eq!("0", var);
    }

    #[test]
    fn displaying_an_app_returns_both_terms_in_parens() {
        let app = format!("{}", Term::App(box Term::Var(0), box Term::Var(1)));

        assert_eq!("(0 1)", app);
    }

    #[test]
    fn displaying_an_abs_returns_a_lambda() {
        let abs = format!("{}", Term::Abs(box Term::Var(0)));

        assert_eq!("(λ. 0)", abs);
    }

    #[test]
    fn displaying_a_nested_expression() {
        let abs = format!("{}", Term::Abs(box Term::Abs(box Term::App(box Term::Var(1), box Term::App(box Term::Var(0), box Term::Var(1))))));

        assert_eq!("(λ. (λ. (1 (0 1))))", abs);
    }

    #[test]
    fn evaluating_a_simple_term() {
        let term = Term::App(
            box Term::Abs(
                box Term::Abs(
                    box Term::App(
                        box Term::Var(1),
                        box Term::Var(0)
                    )
                )
            ),
            box Term::Abs(
                box Term::Var(2)
            )
        );

        let result = eval1(&term).expect("Should not error");

        assert_eq!("((λ. (λ. (1 0))) (λ. 2))", format!("{}", term));
        assert_eq!("(λ. ((λ. 3) 0))", format!("{}", result));
    }
}
