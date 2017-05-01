//! The syntax module contains the definition of the terms of the language.

use std::fmt;

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

#[macro_export]
macro_rules! arith {
    (0) => { Term::Zero };
    (true) => { Term::True };
    (false) => { Term::False };
    (succ $($t1:tt)*) => { Term::Succ(Box::new(arith!($($t1)*))) };
    (pred $($t1:tt)*) => { Term::Pred(Box::new(arith!($($t1)*))) };
    (iszero $($t1:tt)*) => { Term::IsZero(Box::new(arith!($($t1)*))) };
    (if ($($t1:tt)*) then ($($t2:tt)*) else ($($t3:tt)*)) => {
        Term::If(Box::new(arith!($($t1)*)), Box::new(arith!($($t2)*)), Box::new(arith!($($t3)*)))
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_macro_expands_to_zero() {
        let zero = arith! { 0 };

        assert_eq!(zero, Term::Zero);
    }

    #[test]
    fn true_macro_expands_to_true() {
        let truth = arith! { true };

        assert_eq!(truth, Term::True);
    }

    #[test]
    fn false_macro_expands_to_false() {
        let falsehood = arith! { false };

        assert_eq!(falsehood, Term::False);
    }

    #[test]
    fn succ_macro_expands_to_succ() {
        let succ = arith! { succ 0 };

        assert_eq!(succ, Term::Succ(box Term::Zero));
    }

    #[test]
    fn nested_succ_macro_expands_to_succ() {
        let succ = arith! { succ succ 0 };

        assert_eq!(succ, Term::Succ(box Term::Succ(box Term::Zero)));
    }

    #[test]
    fn pred_macro_expands_to_pred() {
        let pred = arith! { pred 0 };

        assert_eq!(pred, Term::Pred(box Term::Zero));
    }

    #[test]
    fn nested_pred_macro_expands_to_pred() {
        let pred = arith! { pred pred 0 };

        assert_eq!(pred, Term::Pred(box Term::Pred(box Term::Zero)));
    }

    #[test]
    fn iszero_macro_expands_to_iszero() {
        let iszero = arith! { iszero 0 };

        assert_eq!(iszero, Term::IsZero(box Term::Zero));
    }

    #[test]
    fn if_macro_expands_to_if() {
        let condition = arith! { if (true) then (true) else (false) };

        assert_eq!(condition, Term::If(box Term::True, box Term::True, box Term::False));
    }

    #[test]
    fn nested_if_macro_expands_to_if() {
        let condition = arith! { if (iszero succ pred 0) then (true) else (false) };

        assert_eq!(condition, Term::If(box Term::IsZero(box Term::Succ(box Term::Pred(box Term::Zero))), box Term::True, box Term::False));
    }
}
