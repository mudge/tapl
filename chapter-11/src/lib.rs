#![warn(missing_docs)]
#![feature(box_syntax, box_patterns)]

//! A Rust implementation of chapter 11 of Benjamin C. Pierce's "Types and Programming Languages"
//! `fullsimple` language.
//!
//! c.f. https://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullsimple/core.ml for the sample OCaml
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
    /// The Unit type.
    Unit,
    /// A Product type.
    Product(Box<Type>, Box<Type>),
    /// A Sum type.
    Sum(Box<Type>, Box<Type>),
    /// A Tuple type.
    Tuple(Vec<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Arrow(box ref t1, box ref t2) => write!(f, "{} → {}", t1, t2),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "Unit"),
            Type::Product(box ref t1, box ref t2) => write!(f, "{}×{}", t1, t2),
            Type::Sum(box ref t1, box ref t2) => write!(f, "{}+{}", t1, t2),
            Type::Tuple(ref ts) => {
                let types: Vec<String> = ts.iter().map(|t| format!("{}", t)).collect();

                write!(f, "{{{}}}", types.join(","))
            }
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
    /// The unit.
    Unit,
    /// A sequence of two terms.
    Sequence(Box<Term>, Box<Term>),
    /// Acription.
    Ascribe(Box<Term>, Type),
    /// A pair.
    Pair(Box<Term>, Box<Term>),
    /// A projection of a pair.
    Project(Box<Term>, usize),
    /// A constructor for the left side of a Sum type
    Inl(Box<Term>, Type),
    /// A constructor for the right side of a Sum type
    Inr(Box<Term>, Type),
    /// A case statement for unwrapping a Sum type
    Case(Box<Term>, String, Box<Term>, String, Box<Term>),
    /// A tuple of terms
    Tuple(Vec<Term>),
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
    /// An incorrect parameter type for an abstraction or ascription.
    ParameterTypeMismatch(Type, Type),
    /// An incorrect type for an application.
    ArrowTypeExpected(Type),
    /// An error if the predicate in a conditional does not result in a boolean.
    GuardOfConditionalNotABoolean(Type),
    /// An error if the two arms of a conditional do not result in the same type.
    ArmsOfConditionalHaveDifferentTypes(Type, Type),
    /// An error if the first term in a sequence is not the unit.
    UnitTypeExpected(Type),
    /// An error when attempting to project something that cannot be projected.
    ProjectableTypeExpected(Type),
    /// An error when attempting to project a field that does not exist.
    InvalidProjection(usize),
    /// An error if a Sum type constructor is used with a non-Sum type.
    SumTypeExpected(Type),
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
            },
            Error::UnitTypeExpected(_) => "first term in a sequence not the unit",
            Error::ProjectableTypeExpected(_) => "invalid type for projection",
            Error::InvalidProjection(_) => "invalid field for projection",
            Error::SumTypeExpected(_) => "sum type expected",
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
            Error::UnitTypeExpected(ref ty) => {
                write!(f, "expected first term in a sequence to be unit, got {}", ty)
            }
            Error::ProjectableTypeExpected(ref ty) => {
                write!(f, "cannot project type {}, must be a Product or Tuple", ty)
            }
            Error::InvalidProjection(i) => {
                write!(f, "cannot project field {}, must be 1 or 2", i)
            }
            Error::SumTypeExpected(ref ty) => write!(f, "expected sum type, got {}", ty),
        }
    }
}

/// A context for type checking with variable names and their bindings.
pub type Context = Vec<(String, Binding)>;

/// Return the type of the given term with the given context.
pub fn type_of(ctx: &[(String, Binding)], t: &Term) -> Result<Type, Error> {
    match *t {
        Term::Tuple(ref ts) => {
            let tys = ts.iter().map(|t| type_of(ctx, t)).collect::<Result<Vec<Type>, Error>>()?;

            Ok(Type::Tuple(tys))
        }
        Term::Inl(box ref t1, ref ty) => {
            match *ty {
                Type::Sum(box ref left, _) => {
                    let ty_t1 = type_of(ctx, t1)?;

                    if left == &ty_t1 {
                        Ok(ty.clone())
                    } else {
                        Err(Error::ParameterTypeMismatch(ty_t1, left.clone()))
                    }
                }
                _ => Err(Error::SumTypeExpected(ty.clone()))
            }
        }
        Term::Inr(box ref t1, ref ty) => {
            match *ty {
                Type::Sum(_, box ref right) => {
                    let ty_t1 = type_of(ctx, t1)?;

                    if right == &ty_t1 {
                        Ok(ty.clone())
                    } else {
                        Err(Error::ParameterTypeMismatch(ty_t1, right.clone()))
                    }
                }
                _ => Err(Error::SumTypeExpected(ty.clone()))
            }
        }
        Term::Case(box ref t0, ref x1, box ref t1, ref x2, box ref t2) => {
            let ty_t0 = type_of(ctx, t0)?;

            match ty_t0 {
                Type::Sum(box left, box right) => {
                    let ctx_with_x1 = add_binding(ctx, x1, Binding::VarBind(left));
                    let ctx_with_x2 = add_binding(ctx, x2, Binding::VarBind(right));
                    let ty_t1 = type_of(&ctx_with_x1, t1)?;
                    let ty_t2 = type_of(&ctx_with_x2, t2)?;

                    if ty_t1 == ty_t2 {
                        Ok(ty_t1)
                    } else {
                        Err(Error::ArmsOfConditionalHaveDifferentTypes(ty_t1, ty_t2))
                    }
                }
                _ => Err(Error::SumTypeExpected(ty_t0))
            }
        }
        Term::Sequence(box ref t1, box ref t2) => {
            let ty_t1 = type_of(ctx, t1)?;

            match ty_t1 {
                Type::Unit => type_of(ctx, t2),
                _ => Err(Error::UnitTypeExpected(ty_t1)),
            }
        }
        Term::Project(box ref t1, i) => {
            let ty_t1 = type_of(ctx, t1)?;

            match ty_t1 {
                Type::Product(box ty_t11, box ty_t12) => {
                    match i {
                        1 => Ok(ty_t11),
                        2 => Ok(ty_t12),
                        _ => Err(Error::InvalidProjection(i)),
                    }
                },
                Type::Tuple(ts) => {
                    ts.get(i - 1).map(|t| t.clone()).ok_or_else(|| Error::InvalidProjection(i))
                }
                _ => Err(Error::ProjectableTypeExpected(ty_t1)),
            }
        }
        Term::Pair(box ref t1, box ref t2) => {
            let ty_t1 = type_of(ctx, t1)?;
            let ty_t2 = type_of(ctx, t2)?;

            Ok(Type::Product(box ty_t1, box ty_t2))
        }
        Term::Ascribe(box ref t1, ref ty) => {
            let ty_t1 = type_of(ctx, t1)?;

            if ty == &ty_t1 {
                Ok(ty_t1)
            } else {
                Err(Error::ParameterTypeMismatch(ty_t1, ty.clone()))
            }
        }
        Term::Unit => Ok(Type::Unit),
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
        Term::Tuple(ref ts) => {
            let terms: Vec<String> = ts.iter().map(|t| print_term(ctx, t)).collect();

            format!("{{{}}}", terms.join(","))
        }
        Term::Inl(box ref t1, ref ty) => {
            format!("inl {} as {}", print_term(ctx, t1), ty)
        }
        Term::Inr(box ref t1, ref ty) => {
            format!("inr {} as {}", print_term(ctx, t1), ty)
        }
        Term::Case(box ref t0, ref x1, box ref t1, ref x2, box ref t2) => {
            let (ctx_with_x1, x1_prime) = pick_fresh_name(ctx, x1);
            let (ctx_with_x2, x2_prime) = pick_fresh_name(ctx, x2);

            format!("case {} of inl {} ⇒ {} | inr {} ⇒ {}", print_term(ctx, t0), x1_prime, print_term(&ctx_with_x1, t1), x2_prime, print_term(&ctx_with_x2, t2))
        }
        Term::Sequence(box ref t1, box ref t2) => {
            format!("{};{}", print_term(ctx, t1), print_term(ctx, t2))
        }
        Term::Project(box ref t1, i) => {
            format!("{}.{}", print_term(ctx, t1), i)
        }
        Term::Pair(box ref t1, box ref t2) => {
            format!("{{{},{}}}", print_term(ctx, t1), print_term(ctx, t2))
        }
        Term::Ascribe(box ref t1, ref ty) => {
            format!("{} as {}", print_term(ctx, t1), ty)
        }
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
        Term::Unit => "unit".into(),
        Term::If(box ref t1, box ref t2, box ref t3) => {
            format!("if {} then {} else {}", t1, t2, t3)
        }
    }
}

#[macro_export]
macro_rules! simplebool {
    ({$t1:tt , $t2:tt}) => { Term::Pair(Box::new(simplebool!($t1)), Box::new(simplebool!($t2))) };
    (($t1:tt as $($ty:tt)+)) => { Term::Ascribe(Box::new(simplebool!($t1)), simplebool!($($ty)*)) };
    (unit) => { Term::Unit };
    (Bool) => { Type::Bool };
    (Unit) => { Type::Unit };
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
    fn type_of_inl() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inl(box Term::True, Type::Sum(box Type::Bool, box Type::Unit)));

        assert_eq!(Ok(Type::Sum(box Type::Bool, box Type::Unit)), ty);
    }

    #[test]
    fn type_of_bad_inl() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inl(box Term::Unit, Type::Sum(box Type::Bool, box Type::Unit)));

        assert_eq!(Err(Error::ParameterTypeMismatch(Type::Unit, Type::Bool)), ty);
    }

    #[test]
    fn type_of_inl_without_sum_type() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inl(box Term::Unit, Type::Bool));

        assert_eq!(Err(Error::SumTypeExpected(Type::Bool)), ty);
    }

    #[test]
    fn type_of_inr() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inr(box Term::True, Type::Sum(box Type::Unit, box Type::Bool)));

        assert_eq!(Ok(Type::Sum(box Type::Unit, box Type::Bool)), ty);
    }

    #[test]
    fn type_of_bad_inr() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inr(box Term::True, Type::Sum(box Type::Bool, box Type::Unit)));

        assert_eq!(Err(Error::ParameterTypeMismatch(Type::Bool, Type::Unit)), ty);
    }

    #[test]
    fn type_of_inr_without_sum_type() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Inr(box Term::Unit, Type::Bool));

        assert_eq!(Err(Error::SumTypeExpected(Type::Bool)), ty);
    }

    #[test]
    fn type_of_case() {
        let ctx = Context::new();
        let istruthy = Term::Case(
            box Term::Inl(box Term::True, Type::Sum(box Type::Bool, box Type::Unit)),
            "b".into(),
            box Term::Var(0),
            "u".into(),
            box Term::False
        );
        let ty = type_of(&ctx, &istruthy);

        assert_eq!(Ok(Type::Bool), ty);
    }

    #[test]
    fn type_of_case_with_mismatched_arms() {
        let ctx = Context::new();
        let istruthy = Term::Case(
            box Term::Inl(box Term::True, Type::Sum(box Type::Bool, box Type::Unit)),
            "b".into(),
            box Term::Var(0),
            "u".into(),
            box Term::Unit
        );
        let ty = type_of(&ctx, &istruthy);

        assert_eq!(Err(Error::ArmsOfConditionalHaveDifferentTypes(Type::Bool, Type::Unit)), ty);
    }

    #[test]
    fn type_of_case_with_non_sum_type() {
        let ctx = Context::new();
        let istruthy = Term::Case(
            box Term::True,
            "b".into(),
            box Term::Var(0),
            "u".into(),
            box Term::Unit
        );
        let ty = type_of(&ctx, &istruthy);

        assert_eq!(Err(Error::SumTypeExpected(Type::Bool)), ty);
    }

    #[test]
    fn type_of_sequence() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Sequence(box Term::Unit, box Term::True));

        assert_eq!(Ok(Type::Bool), ty);
    }

    #[test]
    fn type_of_invalid_sequence() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Sequence(box Term::False, box Term::True));

        assert_eq!(Err(Error::UnitTypeExpected(Type::Bool)), ty);
    }

    #[test]
    fn type_of_pair() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Pair(box Term::True, box Term::Unit));

        assert_eq!(Ok(Type::Product(box Type::Bool, box Type::Unit)), ty);
    }

    #[test]
    fn type_of_ascription() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Ascribe(box Term::True, Type::Bool));

        assert_eq!(Ok(Type::Bool), ty);
    }

    #[test]
    fn invalid_ascription_raises_type_error() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Ascribe(box Term::True, Type::Unit));

        assert!(ty.is_err());
    }

    #[test]
    fn unit_is_the_unit_type() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Unit);

        assert_eq!(Ok(Type::Unit), ty);
    }

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

    #[test]
    fn displaying_a_tuple() {
        let tuple = Type::Tuple(vec![Type::Bool, Type::Unit, Type::Arrow(box Type::Bool, box Type::Bool)]);

        assert_eq!("{Bool, Unit, Bool → Bool}", format!("{}", tuple));
    }

    #[test]
    fn type_of_tuple() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Tuple(vec![Term::True, Term::False, Term::Unit]));

        assert_eq!(Ok(Type::Tuple(vec![Type::Bool, Type::Bool, Type::Unit])), ty);
    }

    #[test]
    fn projecting_a_tuple() {
        let ctx = Context::new();
        let ty = type_of(&ctx, &Term::Project(box Term::Tuple(vec![Term::True, Term::False, Term::Unit]), 1));

        assert_eq!(Ok(Type::Bool), ty);
    }
}
