//! The evaluation rules for this language.

use {Error, Result, Term};
use Term::*;

/// Evaluate a term according to the evaluation rules until no further rules apply.
///
/// # Example
///
/// ```rust
/// #![feature(box_syntax, box_patterns)]
/// use arith::eval;
/// use arith::Term::*;
///
/// assert_eq!(True, eval(If(box IsZero(box Zero), box True, box False)));
/// ```
pub fn eval(t: Term) -> Term {
    match eval1(t.clone()) {
        Ok(t_prime) => eval(t_prime),
        _ => t,
    }
}

/// Evaluate a single evaluation rule against a given `Term` returning either a new `Term` or an
/// `Error` if no further rules apply.
fn eval1(t: Term) -> Result<Term> {
    match t {
        If(box True, t2, _) => Ok(*t2),
        If(box False, _, t3) => Ok(*t3),
        If(t1, t2, t3) => {
            let t_prime = eval1(*t1)?;

            Ok(If(box t_prime, t2, t3))
        }
        Succ(t1) => {
            let t_prime = eval1(*t1)?;

            Ok(Succ(box t_prime))
        }
        Pred(box Zero) => Ok(Zero),
        Pred(box Succ(ref nv1)) if is_numeric_val(nv1.as_ref()) => Ok(*(nv1.clone())),
        Pred(t1) => {
            let t_prime = eval1(*t1)?;

            Ok(Pred(box t_prime))
        }
        IsZero(box Zero) => Ok(True),
        IsZero(box Succ(ref nv1)) if is_numeric_val(nv1.as_ref()) => Ok(False),
        IsZero(t1) => {
            let t_prime = eval1(*t1)?;

            Ok(IsZero(box t_prime))
        }
        _ => Err(Error::NoRuleApplies(t)),
    }
}

/// Return whether a given `Term` is a value or not.
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
        Succ(ref t1) => is_numeric_val(t1.as_ref()),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use Term::*;
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
    fn is_val_returns_false_for_conditionals() {
        assert!(!is_val(&If(box True, box True, box True)));
    }

    #[test]
    fn eval1_evaluates_if_true_to_consequent() {
        let if_true = If(box True, box True, box False);
        let evaluation = eval1(if_true).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_false_to_alternate() {
        let if_false = If(box False, box True, box False);
        let evaluation = eval1(if_false).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_iszero_zero_to_if_true() {
        let if_iszero_zero = If(box IsZero(box Zero),
                                box True,
                                box False);
        let evaluation = eval1(if_iszero_zero).expect("Should not error");

        assert_eq!(If(box True, box True, box False),
                   evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_zero_to_true() {
        let iszero_zero = IsZero(box Zero);
        let evaluation = eval1(iszero_zero).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_succ_to_false() {
        let iszero_succ = IsZero(box Succ(box Zero));
        let evaluation = eval1(iszero_succ).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_pred_zero_to_iszero_zero() {
        let iszero_pred_zero = IsZero(box Pred(box Zero));
        let evaluation = eval1(iszero_pred_zero).expect("Should not error");

        assert_eq!(IsZero(box Zero), evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_zero_to_zero() {
        let pred_zero = Pred(box Zero);
        let evaluation = eval1(pred_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_succ_zero_to_zero() {
        let pred_succ_zero = Pred(box Succ(box Zero));
        let evaluation = eval1(pred_succ_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval_evaluates_iszero_pred_zero_to_true() {
        let iszero_pred_zero = IsZero(box Pred(box Zero));

        assert_eq!(True, eval(iszero_pred_zero));
    }

    #[test]
    fn eval_evaluates_succ_zero_to_itself() {
        let succ_zero = Succ(box Zero);

        assert_eq!(Succ(box Zero), eval(succ_zero));
    }
}
