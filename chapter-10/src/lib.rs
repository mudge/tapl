#![warn(missing_docs)]
#![feature(box_syntax, box_patterns)]

//! A Rust implementation of chapter 10 of Benjamin C. Pierce's "Types and Programming Languages"
//! `simplebool` language.
//!
//! c.f. https://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool/core.ml for the sample OCaml
//! implementation.

use std::error;
use std::fmt;

/// Available types in this language.
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    /// The type of a function from a input type to a result type.
    Arrow(Box<Type>, Box<Type>),
    /// A boolean.
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Arrow(box ref t1, box ref t2) => write!(f, "{} → {}", t1, t2),
            Type::Bool => write!(f, "Bool"),
        }
    }
}

/// A term in the untyped Lambda Calculus.
#[derive(PartialEq, Debug, Clone)]
pub enum Term {
    /// A variable and its de Bruijn index.
    Var(usize),
    /// An abstraction's body, type of argument and a hint to its name (e.g. "x").
    Abs(String, Type, Box<Term>),
    /// An application of two terms.
    App(Box<Term>, Box<Term>),
    /// The boolean true.
    True,
    /// The boolean false.
    False,
    /// An if statement with a condition, consequent and alternate.
    If(Box<Term>, Box<Term>, Box<Term>),
}

/// The various types of bindings available in a `Context`.
#[derive(PartialEq, Debug, Clone)]
pub enum Binding {
    /// A name binding.
    NameBind,
    /// A variable binding to a specific type.
    VarBind(Type),
}

/// Possible errors while type checking.
#[derive(PartialEq, Debug, Clone)]
pub enum Error {
    /// An incorrect binding for a variable.
    WrongBinding(usize),
    /// An incorrect parameter type for an abstraction.
    ParameterTypeMismatch(Type, Type),
    /// An incorrect type for an application.
    ArrowTypeExpected(Type),
    /// An error if the predicate in a conditional does not result in a boolean.
    GuardOfConditionalNotABoolean(Type),
    /// An error if the two arms of a conditional do not result in the same type.
    ArmsOfConditionalHaveDifferentTypes(Type, Type),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::WrongBinding(_) => "wrong binding for a variable",
            Error::ParameterTypeMismatch(_, _) => "parameter type mismatch",
            Error::ArrowTypeExpected(_) => "arrow type expected",
            Error::GuardOfConditionalNotABoolean(_) => "guard of conditional not a boolean",
            Error::ArmsOfConditionalHaveDifferentTypes(_, _) => {
                "arms of conditional have different types"
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::WrongBinding(i) => write!(f, "wrong binding for variable with index {}", i),
            Error::ParameterTypeMismatch(ref ty1, ref ty2) => {
                write!(f,
                       "parameter type mismatch, expected {} to match {}",
                       ty1,
                       ty2)
            }
            Error::ArrowTypeExpected(ref ty) => write!(f, "expected arrow type, got {}", ty),
            Error::GuardOfConditionalNotABoolean(ref ty) => {
                write!(f, "expected guard of conditional to be boolean, got {}", ty)
            }
            Error::ArmsOfConditionalHaveDifferentTypes(ref ty1, ref ty2) => {
                write!(f,
                       "arms of conditional have different types, expected {} to match {}",
                       ty1,
                       ty2)
            }
        }
    }
}

/// A context for type checking with variable names and their bindings.
pub type Context = Vec<(String, Binding)>;

/// Return the type of the given term with the given context.
pub fn type_of(ctx: &[(String, Binding)], t: &Term) -> Result<Type, Error> {
    match *t {
        Term::Var(i) => get_type_from_context(ctx, i),
        Term::Abs(ref x, ref ty_t1, box ref t2) => {
            let ctx_prime = add_binding(ctx, x, Binding::VarBind(ty_t1.clone()));
            let ty_t2 = type_of(&ctx_prime, t2)?;

            Ok(Type::Arrow(box ty_t1.clone(), box ty_t2))
        }
        Term::App(box ref t1, box ref t2) => {
            let ty_t1 = type_of(ctx, t1)?;
            let ty_t2 = type_of(ctx, t2)?;

            match ty_t1 {
                Type::Arrow(box ty_t11, box ty_t12) => {
                    if ty_t2 == ty_t11 {
                        Ok(ty_t12)
                    } else {
                        Err(Error::ParameterTypeMismatch(ty_t2, ty_t11))
                    }
                }
                _ => Err(Error::ArrowTypeExpected(ty_t1)),
            }
        }
        Term::True | Term::False => Ok(Type::Bool),
        Term::If(box ref t1, box ref t2, box ref t3) => {
            let ty_t1 = type_of(ctx, t1)?;

            if ty_t1 == Type::Bool {
                let ty_t2 = type_of(ctx, t2)?;
                let ty_t3 = type_of(ctx, t3)?;

                if ty_t2 == ty_t3 {
                    Ok(ty_t2)
                } else {
                    Err(Error::ArmsOfConditionalHaveDifferentTypes(ty_t2, ty_t3))
                }
            } else {
                Err(Error::GuardOfConditionalNotABoolean(ty_t1))
            }
        }
    }
}

fn get_type_from_context(ctx: &[(String, Binding)], i: usize) -> Result<Type, Error> {
    match get_binding(ctx, i) {
        Some(Binding::VarBind(ty_t)) => Ok(ty_t.clone()),
        _ => Err(Error::WrongBinding(i)),
    }
}

fn add_binding(ctx: &[(String, Binding)], x: &str, bind: Binding) -> Context {
    let mut ctx_prime = ctx.to_vec();
    ctx_prime.insert(0, (x.into(), bind));

    ctx_prime
}

fn get_binding(ctx: &[(String, Binding)], i: usize) -> Option<Binding> {
    ctx.get(i).map(|&(_, ref bind)| bind.clone())
}

fn is_name_bound(ctx: &[(String, Binding)], x: &str) -> bool {
    match ctx.split_first() {
        Some((&(ref y, _), _)) if x == y => true,
        Some((_, rest)) => is_name_bound(rest, x),
        None => false,
    }
}

fn pick_fresh_name(ctx: &[(String, Binding)], x: &str) -> (Context, String) {
    if is_name_bound(ctx, x) {
        let x_prime = format!("{}'", x);

        pick_fresh_name(ctx, &x_prime)
    } else {
        (add_binding(ctx, x, Binding::NameBind), x.into())
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ctx = Context::new();

        write!(f, "{}", print_term(&ctx, self))
    }
}

/// Pretty-print a `Term` with a given `Context` rather than using de Bruijn indices.
fn print_term(ctx: &[(String, Binding)], t: &Term) -> String {
    match *t {
        Term::Abs(ref x, ref ty, box ref t1) => {
            let (ctx_prime, x_prime) = pick_fresh_name(ctx, x);

            format!("(λ{}:{}. {})", x_prime, ty, print_term(&ctx_prime, t1))
        }
        Term::App(box ref t1, box ref t2) => {
            format!("({} {})", print_term(ctx, t1), print_term(ctx, t2))
        }
        Term::Var(x) => {
            ctx.get(x).map(|&(ref name, _)| name.clone()).unwrap_or_else(|| "[bad index]".into())
        }
        Term::True => "true".into(),
        Term::False => "false".into(),
        Term::If(box ref t1, box ref t2, box ref t3) => {
            format!("if {} then {} else {}", t1, t2, t3)
        }
    }
}

#[macro_export]
macro_rules! simplebool {
    (Bool) => { Type::Bool };
    (true) => { Term::True };
    (false) => { Term::False };
    ((if $t1:tt then $t2:tt else $t3:tt)) => {
        Term::If(Box::new(simplebool!($t1)), Box::new(simplebool!($t2)), Box::new(simplebool!($t3)))
    };
    ((λ ($x:ident : $($ty:tt)+) . $t1:tt)) => {
        Term::Abs(stringify!($x).into(), simplebool!($($ty)*), Box::new(simplebool!($t1)))
    };
    (($t1:tt $t2:tt)) => { Term::App(Box::new(simplebool!($t1)), Box::new(simplebool!($t2))) };
    ($x:expr) => { Term::Var($x) };
    ($ty1:ident -> $($ty2:tt)*) => {
        Type::Arrow(Box::new(simplebool!($ty1)), Box::new(simplebool!($($ty2)*)))
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simplebool_expands_bool_to_type() {
        let ty = simplebool! { Bool };

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn simplebool_expands_arrow_to_arrow_type() {
        let ty = simplebool! { Bool -> Bool };

        assert_eq!(Type::Arrow(box Type::Bool, box Type::Bool), ty);
    }

    #[test]
    fn simplebool_expands_nested_arrows() {
        let ty = simplebool! { Bool -> Bool -> Bool -> Bool };

        assert_eq!(Type::Arrow(box Type::Bool,
                               box Type::Arrow(box Type::Bool,
                                               box Type::Arrow(box Type::Bool, box Type::Bool))),
                   ty);
    }

    #[test]
    fn simplebool_expands_abstractions_with_type_annotations() {
        let term = simplebool! { (λ (x : Bool) . 0) };

        assert_eq!(Term::Abs("x".into(), Type::Bool, box Term::Var(0)), term);
    }

    #[test]
    fn simplebool_expands_abstractions_with_arrow_type_annotations() {
        let term = simplebool! { (λ (x : Bool -> Bool) . 0) };

        assert_eq!(Term::Abs("x".into(),
                             Type::Arrow(box Type::Bool, box Type::Bool),
                             box Term::Var(0)),
                   term);
    }

    #[test]
    fn simplebool_expands_abstractions_with_nested_arrow_type_annotations() {
        let term = simplebool! { (λ (x : Bool -> Bool -> Bool) . 0) };

        assert_eq!(Term::Abs("x".into(),
                             Type::Arrow(box Type::Bool,
                                         box Type::Arrow(box Type::Bool, box Type::Bool)),
                             box Term::Var(0)),
                   term);
    }

    #[test]
    fn simplebool_expands_applications() {
        let term = simplebool! { ((λ (x : Bool) . 0) true) };

        assert_eq!(Term::App(box Term::Abs("x".into(), Type::Bool, box Term::Var(0)),
                             box Term::True),
                   term);
    }

    #[test]
    fn simplebool_expands_applications_with_more_complicated_terms() {
        let term = simplebool! { ((λ (x : Bool) . 0) (if true then true else false)) };

        assert_eq!(Term::App(box Term::Abs("x".into(), Type::Bool, box Term::Var(0)),
                             box Term::If(box Term::True, box Term::True, box Term::False)),
                   term);
    }

    #[test]
    fn simplebool_expands_nested_conditionals() {
        let term = simplebool! { (if (if true then true else false) then false else true) };

        assert_eq!(Term::If(box Term::If(box Term::True, box Term::True, box Term::False),
                            box Term::False,
                            box Term::True),
                   term);
    }

    #[test]
    fn add_binding_to_an_empty_context() {
        let ctx = Context::new();
        let ctx_prime = add_binding(&ctx, "x", Binding::NameBind);

        assert_eq!(vec![("x".into(), Binding::NameBind)], ctx_prime);
    }

    #[test]
    fn add_binding_prepends_to_a_nonempty_context() {
        let ctx: Context = vec![("x".into(), Binding::NameBind)];
        let ctx_prime = add_binding(&ctx, "y", Binding::NameBind);

        assert_eq!(vec![("y".into(), Binding::NameBind), ("x".into(), Binding::NameBind)],
                   ctx_prime);
    }

    #[test]
    fn get_binding_returns_the_nth_binding() {
        let ctx: Context = vec![("x".into(), Binding::NameBind),
                                ("y".into(), Binding::VarBind(Type::Bool))];
        let bind = get_binding(&ctx, 1).expect("Should not panic");

        assert_eq!(Binding::VarBind(Type::Bool), bind);
    }

    #[test]
    fn get_binding_returns_none_if_no_binding_exists_at_that_index() {
        let ctx = Context::new();
        let bind = get_binding(&ctx, 15);

        assert!(bind.is_none());
    }

    #[test]
    fn get_type_from_context_returns_type_of_var_binding() {
        let ctx: Context = vec![("x".into(), Binding::VarBind(Type::Bool))];
        let ty = get_type_from_context(&ctx, 0).expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn get_type_from_context_returns_error_if_not_a_var_binding() {
        let ctx: Context = vec![("x".into(), Binding::NameBind)];
        let ty = get_type_from_context(&ctx, 0);

        assert!(ty.is_err());
    }

    #[test]
    fn type_of_var_is_its_binding() {
        let ctx: Context = vec![("x".into(), Binding::VarBind(Type::Bool))];
        let ty = type_of(&ctx, &Term::Var(0)).expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn type_of_abs_is_arrow_of_input_to_output() {
        let ctx = Context::new();
        let term = simplebool! { (λ (x: Bool) . 0) };
        let ty = type_of(&ctx, &term).expect("Should not panic");

        assert_eq!(Type::Arrow(box Type::Bool, box Type::Bool), ty);
    }

    #[test]
    fn type_of_abs_works_with_nested_abstractions() {
        let ctx = Context::new();
        let term = simplebool! { (λ (x: Bool) . (λ (y: Bool) . 0)) };
        let ty = type_of(&ctx, &term).expect("Should not panic");

        assert_eq!(Type::Arrow(box Type::Bool,
                               box Type::Arrow(box Type::Bool, box Type::Bool)),
                   ty);
    }

    #[test]
    fn type_of_app_is_return_type_of_abs() {
        let ctx = Context::new();
        let term = simplebool! { ((λ (x: Bool) . 0) true) };
        let ty = type_of(&ctx, &term).expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn type_of_app_errors_if_not_given_an_abs() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::App(box Term::False, box Term::True));

        assert!(ty.is_err());
    }

    #[test]
    fn type_of_app_errors_if_given_the_wrong_type() {
        let ctx = Context::new();
        let ty = type_of(&ctx,
                         &Term::App(box Term::Abs("x".into(), Type::Bool, box Term::Var(0)),
                                    box Term::Var(0)));

        assert!(ty.is_err());
    }

    #[test]
    fn type_of_true_is_bool() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::True).expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn type_of_false_is_bool() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::False).expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn type_of_if_is_type_of_arms() {
        let ctx = Context::new();
        let ty = type_of(&ctx,
                         &Term::If(box Term::True, box Term::True, box Term::False))
            .expect("Should not panic");

        assert_eq!(Type::Bool, ty);
    }

    #[test]
    fn type_of_if_errors_if_arms_do_not_match() {
        let ctx = Context::new();
        let ty = type_of(&ctx,
                         &Term::If(box Term::True, box Term::True, box Term::Var(0)));

        assert!(ty.is_err());
    }

    #[test]
    fn type_of_if_errors_if_guard_is_not_bool() {
        let ctx = Context::new();
        let ty = type_of(&ctx,
                         &Term::If(box Term::Var(0), box Term::True, box Term::False));

        assert!(ty.is_err());
    }
}
