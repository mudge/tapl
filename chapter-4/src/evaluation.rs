//! The evaluation rules for this language.

use Term;
use Term::{True, False, If, Zero, Succ, Pred, IsZero};

/// Evaluate a term according to the evaluation rules until no further rules apply.
///
/// # Example
///
/// ```rust
/// #![feature(box_syntax, box_patterns)]
/// use arith::eval;
/// use arith::Term::*;
///
/// let term = If(box IsZero(box Zero), box True, box False);
/// assert_eq!(True, eval(&term));
/// ```
pub fn eval(t: &Term) -> Term {
    eval1(t).map_or(t.clone(), |t_prime| eval(&t_prime))
}

/// Evaluate a single evaluation rule against a given `Term` returning either a some new `Term` or
/// nothing if no further rules apply.
fn eval1(t: &Term) -> Option<Term> {
    match *t {
        If(box True, box ref t2, _) => Some(t2.clone()),
        If(box False, _, box ref t3) => Some(t3.clone()),
        If(box ref t1, ref t2, ref t3) => eval1(t1).map(|t_prime| If(box t_prime, t2.clone(), t3.clone())),
        Succ(box ref t1) => eval1(t1).map(|t_prime| Succ(box t_prime)),
        Pred(box Zero) => Some(Zero),
        Pred(box Succ(box ref nv1)) if is_numeric_val(nv1) => Some(nv1.clone()),
        Pred(box ref t1) => eval1(t1).map(|t_prime| Pred(box t_prime)),
        IsZero(box Zero) => Some(True),
        IsZero(box Succ(box ref nv1)) if is_numeric_val(nv1) => Some(False),
        IsZero(box ref t1) => eval1(t1).map(|t_prime| IsZero(box t_prime)),
        _ => None,
    }
}

/// Return whether a given `Term` is a value or not.
#[allow(dead_code)]
fn is_val(t: &Term) -> bool {
    match *t {
        True | False => true,
        ref t if is_numeric_val(t) => true,
        _ => false,
    }
}

/// Return whether a given `Term` is a numeric value or not.
fn is_numeric_val(t: &Term) -> bool {
    match *t {
        Zero => true,
        Succ(box ref t1) => is_numeric_val(t1),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_numeric_val_returns_true_for_zero() {
        assert!(is_numeric_val(&Zero));
    }

    #[test]
    fn is_numeric_val_returns_true_for_succ_zero() {
        assert!(is_numeric_val(&Succ(box Zero)));
    }

    #[test]
    fn is_numeric_val_returns_false_for_true() {
        assert!(!is_numeric_val(&True));
    }

    #[test]
    fn is_numeric_val_returns_false_for_invalid_succ() {
        assert!(!is_numeric_val(&Succ(box False)));
    }

    #[test]
    fn is_val_returns_true_for_true() {
        assert!(is_val(&True));
    }

    #[test]
    fn is_val_returns_true_for_false() {
        assert!(is_val(&False));
    }

    #[test]
    fn is_val_returns_true_for_zero() {
        assert!(is_val(&Zero));
    }

    #[test]
    fn is_val_returns_true_for_succ_zero() {
        assert!(is_val(&Succ(box Zero)));
    }

    #[test]
    fn is_val_returns_true_for_succ_succ_zero() {
        assert!(is_val(&Succ(box Succ(box Zero))));
    }

    #[test]
    fn is_val_returns_false_for_conditionals() {
        assert!(!is_val(&If(box True, box True, box True)));
    }

    #[test]
    fn eval1_evaluates_if_true_to_consequent() {
        let if_true = If(box True, box True, box False);
        let evaluation = eval1(&if_true).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_false_to_alternate() {
        let if_false = If(box False, box True, box False);
        let evaluation = eval1(&if_false).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_iszero_zero_to_if_true() {
        let if_iszero_zero = If(box IsZero(box Zero), box True, box False);
        let evaluation = eval1(&if_iszero_zero).expect("Should not error");

        assert_eq!(If(box True, box True, box False), evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_zero_to_true() {
        let iszero_zero = IsZero(box Zero);
        let evaluation = eval1(&iszero_zero).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_succ_to_false() {
        let iszero_succ = IsZero(box Succ(box Zero));
        let evaluation = eval1(&iszero_succ).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_pred_zero_to_iszero_zero() {
        let iszero_pred_zero = IsZero(box Pred(box Zero));
        let evaluation = eval1(&iszero_pred_zero).expect("Should not error");

        assert_eq!(IsZero(box Zero), evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_zero_to_zero() {
        let pred_zero = Pred(box Zero);
        let evaluation = eval1(&pred_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_succ_zero_to_zero() {
        let pred_succ_zero = Pred(box Succ(box Zero));
        let evaluation = eval1(&pred_succ_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval_evaluates_iszero_pred_zero_to_true() {
        let iszero_pred_zero = IsZero(box Pred(box Zero));

        assert_eq!(True, eval(&iszero_pred_zero));
    }

    #[test]
    fn eval_evaluates_succ_zero_to_itself() {
        let succ_zero = Succ(box Zero);

        assert_eq!(Succ(box Zero), eval(&succ_zero));
    }
}
