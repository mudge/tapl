//! The evaluation rules for this language.

use {Error, Result, Term};
use Term::*;

/// Evaluate a term according to the evaluation rules until no further rules apply.
///
/// # Example
///
/// ```rust
/// use arith::eval;
/// use arith::Term::*;
///
/// assert_eq!(True, eval(If(Box::new(IsZero(Box::new(Zero))), Box::new(True), Box::new(False))));
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
        If(t1, t2, t3) => {
            match *t1 {
                True => Ok(*t2),
                False => Ok(*t3),
                _ => {
                    let t_prime = eval1(*t1)?;

                    Ok(If(Box::new(t_prime), t2, t3))
                }
            }
        }
        Succ(t1) => {
            let t_prime = eval1(*t1)?;

            Ok(Succ(Box::new(t_prime)))
        }
        Pred(t1) => {
            match *t1 {
                Zero => Ok(Zero),
                Succ(ref nv1) if is_numeric_val(nv1.as_ref()) => Ok(*(nv1.clone())),
                _ => {
                    let t_prime = eval1(*t1)?;

                    Ok(Pred(Box::new(t_prime)))
                }
            }
        }
        IsZero(t1) => {
            match *t1 {
                Zero => Ok(True),
                Succ(ref nv1) if is_numeric_val(nv1.as_ref()) => Ok(False),
                _ => {
                    let t_prime = eval1(*t1)?;

                    Ok(IsZero(Box::new(t_prime)))
                }
            }
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
        assert!(is_numeric_val(&Succ(Box::new(Zero))));
    }

    #[test]
    fn is_numeric_val_returns_false_for_true() {
        assert!(!is_numeric_val(&True));
    }

    #[test]
    fn is_numeric_val_returns_false_for_invalid_succ() {
        assert!(!is_numeric_val(&Succ(Box::new(False))));
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
        assert!(is_val(&Succ(Box::new(Zero))));
    }

    #[test]
    fn is_val_returns_false_for_conditionals() {
        assert!(!is_val(&If(Box::new(True), Box::new(True), Box::new(True))));
    }

    #[test]
    fn eval1_evaluates_if_true_to_consequent() {
        let if_true = If(Box::new(True), Box::new(True), Box::new(False));
        let evaluation = eval1(if_true).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_false_to_alternate() {
        let if_false = If(Box::new(False), Box::new(True), Box::new(False));
        let evaluation = eval1(if_false).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_iszero_zero_to_if_true() {
        let if_iszero_zero = If(Box::new(IsZero(Box::new(Zero))),
                                Box::new(True),
                                Box::new(False));
        let evaluation = eval1(if_iszero_zero).expect("Should not error");

        assert_eq!(If(Box::new(True), Box::new(True), Box::new(False)),
                   evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_zero_to_true() {
        let iszero_zero = IsZero(Box::new(Zero));
        let evaluation = eval1(iszero_zero).expect("Should not error");

        assert_eq!(True, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_succ_to_false() {
        let iszero_succ = IsZero(Box::new(Succ(Box::new(Zero))));
        let evaluation = eval1(iszero_succ).expect("Should not error");

        assert_eq!(False, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_pred_zero_to_iszero_zero() {
        let iszero_pred_zero = IsZero(Box::new(Pred(Box::new(Zero))));
        let evaluation = eval1(iszero_pred_zero).expect("Should not error");

        assert_eq!(IsZero(Box::new(Zero)), evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_zero_to_zero() {
        let pred_zero = Pred(Box::new(Zero));
        let evaluation = eval1(pred_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_succ_zero_to_zero() {
        let pred_succ_zero = Pred(Box::new(Succ(Box::new(Zero))));
        let evaluation = eval1(pred_succ_zero).expect("Should not error");

        assert_eq!(Zero, evaluation);
    }

    #[test]
    fn eval_evaluates_iszero_pred_zero_to_true() {
        let iszero_pred_zero = IsZero(Box::new(Pred(Box::new(Zero))));

        assert_eq!(True, eval(iszero_pred_zero));
    }

    #[test]
    fn eval_evaluates_succ_zero_to_itself() {
        let succ_zero = Succ(Box::new(Zero));

        assert_eq!(Succ(Box::new(Zero)), eval(succ_zero));
    }
}
