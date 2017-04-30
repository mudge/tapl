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
    Abs(String, Box<Term>),
    /// Application.
    App(Box<Term>, Box<Term>),
}

type Context = Vec<String>;

/// Pretty-print a `Term` with a given `Context` rather than using de Bruijn indices.
pub fn print_term(ctx: &Context, t: &Term) -> String {
    match *t {
        Term::Abs(ref x, box ref t1) => {
            let (ctx_prime, x_prime) = pick_fresh_name(ctx, x);

            format!("(λ{}. {})", x_prime, print_term(&ctx_prime, t1))
        }
        Term::App(box ref t1, box ref t2) => format!("({} {})", print_term(ctx, t1), print_term(ctx, t2)),
        Term::Var(x) => ctx.get(x as usize).map(|name| name.clone()).unwrap_or("[bad index]".to_owned()),
    }
}

/// Evaluate a given `Term` until no more evaluation rules apply.
pub fn eval(t: &Term) -> Term {
    eval1(t).map_or(t.clone(), |t_prime| eval(&t_prime))
}

fn eval1(t: &Term) -> Option<Term> {
    match *t {
        Term::App(box Term::Abs(_, box ref t12), box ref v2) if is_val(v2) => Some(term_subst_top(v2, t12)),
        Term::App(box ref v1, box ref t2) if is_val(v1) => eval1(t2).map(|t2_prime| Term::App(box v1.clone(), box t2_prime)),
        Term::App(box ref t1, box ref t2) => eval1(t1).map(|t1_prime| Term::App(box t1_prime, box t2.clone())),
        _ => None,
    }
}

fn is_val(t: &Term) -> bool {
    match *t {
        Term::Abs(_, _) => true,
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
        Term::Abs(ref x, ref t1) => Term::Abs(x.clone(), box term_subst_walk(c + 1, t1, j, s)),
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
        Term::Abs(ref x, ref t1) => Term::Abs(x.clone(), box term_shift_walk(c + 1, t1, d)),
        Term::App(ref t1, ref t2) => Term::App(box term_shift_walk(c, t1, d), box term_shift_walk(c, t2, d)),
    }
}

fn pick_fresh_name(ctx: &Context, x: &str) -> (Context, String) {
    let mut ctx_prime = ctx.clone();

    if ctx_prime.contains(&x.to_owned()) {
        let x_prime = format!("{}'", x);

        pick_fresh_name(ctx, &x_prime)
    } else {
        ctx_prime.push(x.to_owned());

        (ctx_prime, x.to_owned())
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(x) => write!(f, "{}", x),
            Term::App(ref t1, ref t2) => write!(f, "({} {})", t1, t2),
            Term::Abs(_, ref t1) => write!(f, "(λ. {})", t1),
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
        assert!(is_val(&Term::Abs("x".to_owned(), box Term::Var(0))));
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
        let abs = format!("{}", Term::Abs("x".to_owned(), box Term::Var(0)));

        assert_eq!("(λ. 0)", abs);
    }

    #[test]
    fn displaying_a_nested_expression() {
        let abs = format!(
            "{}",
            Term::Abs(
                "x".to_owned(),
                box Term::Abs(
                    "y".to_owned(),
                    box Term::App(box Term::Var(1), box Term::App(box Term::Var(0), box Term::Var(1)))
                )
            )
        );

        assert_eq!("(λ. (λ. (1 (0 1))))", abs);
    }

    #[test]
    fn evaluating_a_simple_term() {
        let term = Term::App(
            box Term::Abs(
                "x".to_owned(),
                box Term::Abs(
                    "y".to_owned(),
                    box Term::App(
                        box Term::Var(1),
                        box Term::Var(0)
                    )
                )
            ),
            box Term::Abs(
                "z".to_owned(),
                box Term::Var(0)
            )
        );

        let result = eval1(&term).expect("Should not error");

        assert_eq!("((λ. (λ. (1 0))) (λ. 0))", format!("{}", term));
        assert_eq!("(λ. ((λ. 0) 0))", format!("{}", result));
    }

    #[test]
    fn evaluating_a_more_complicated_term() {
        let term = Term::App(
            box Term::Abs("x".to_owned(), box Term::Var(0)),
            box Term::App(
                box Term::Abs("x".to_owned(), box Term::Var(0)),
                box Term::Abs(
                    "z".to_owned(),
                    box Term::App(
                        box Term::Abs("x".to_owned(), box Term::Var(1)),
                        box Term::Var(0)
                    )
                )
            )
        );

        let result = eval1(&term).expect("Should not error");
        let result_2 = eval1(&result).expect("Should not error");

        assert_eq!("((λ. 0) ((λ. 0) (λ. ((λ. 1) 0))))", format!("{}", term));
        assert_eq!("((λ. 0) (λ. ((λ. 1) 0)))", format!("{}", result));
        assert_eq!("(λ. ((λ. 1) 0))", format!("{}", result_2));
        assert_eq!("(λ. ((λ. 1) 0))", format!("{}", eval(&term)));
    }

    #[test]
    fn pick_fresh_name_returns_a_context_and_name() {
        let ctx = Vec::new();
        let (ctx_prime, name) = pick_fresh_name(&ctx, "x");

        assert_eq!(vec!["x".to_owned()], ctx_prime);
        assert_eq!("x".to_owned(), name);
    }

    #[test]
    fn pick_fresh_name_returns_a_context_and_name_with_a_nonempty_context() {
        let ctx = vec!["x".to_owned()];
        let (ctx_prime, name) = pick_fresh_name(&ctx, "y");

        assert_eq!(vec!["x".to_owned(), "y".to_owned()], ctx_prime);
        assert_eq!("y".to_owned(), name);
    }

    #[test]
    fn pick_fresh_name_returns_a_context_and_name_when_a_name_already_exists() {
        let ctx = vec!["x".to_owned()];
        let (ctx_prime, name) = pick_fresh_name(&ctx, "x");

        assert_eq!(vec!["x".to_owned(), "x'".to_owned()], ctx_prime);
        assert_eq!("x'".to_owned(), name);
    }

    #[test]
    fn print_pretty_prints_a_term_with_a_context() {
        let ctx = Vec::new();
        let term = Term::Abs("x".to_owned(), box Term::Var(0));

        let result = print_term(&ctx, &term);

        assert_eq!("(λx. x)", result);
    }

    #[test]
    fn print_pretty_prints_more_complicated_terms() {
        let ctx = Vec::new();
        let term = Term::App(
            box Term::Abs("x".to_owned(), box Term::Var(0)),
            box Term::App(
                box Term::Abs("x".to_owned(), box Term::Var(0)),
                box Term::Abs(
                    "z".to_owned(),
                    box Term::App(
                        box Term::Abs("x".to_owned(), box Term::Var(1)),
                        box Term::Var(0)
                    )
                )
            )
        );

        let result = print_term(&ctx, &term);

        assert_eq!("((λ. 0) ((λ. 0) (λ. ((λ. 1) 0))))", format!("{}", term));
        assert_eq!("((λx. x) ((λx. x) (λz. ((λx. x) z))))", result);
    }
}
