//! The syntax module contains the definition of the terms of the language.

use std::fmt;

/// Types of the language.
#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    /// The type of booleans
    Bool,
    /// The type of Natural numbers
    Nat,
}

/// Terms of the language.
#[derive(PartialEq, Debug, Clone)]
pub enum Term {
    /// The term for boolean `true`
    True,
    /// The term for boolean `false`
    False,
    /// The term for the conditional `if t1 then t2 else t3`
    If(Box<Term>, Box<Term>, Box<Term>),
    /// The term for the number 0
    Zero,
    /// The term for successor `succ t1`
    Succ(Box<Term>),
    /// The term for predecessor `pred t1`
    Pred(Box<Term>),
    /// The term for the predicate `iszero t1`
    IsZero(Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::If(ref t1, ref t2, ref t3) => write!(f, "if {} then {} else {}", t1, t2, t3),
            Term::Zero => write!(f, "0"),
            Term::Succ(ref t1) => write!(f, "succ {}", t1),
            Term::Pred(ref t1) => write!(f, "pred {}", t1),
            Term::IsZero(ref t1) => write!(f, "iszero {}", t1),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
        }
    }
}
