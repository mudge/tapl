//! The syntax module contains the definition of the terms of the language.

use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Term {
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
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
