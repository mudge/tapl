//! The typing rules for this language.

use Term;
use Term::{True, False, If, Zero, Succ, Pred, IsZero};
use Type;
use {Error, Result};

/// Determine the type of a given `Term` returning either a `Type` or
/// an `Error` if the term is not well-typed.
pub fn type_of(t: &Term) -> Result<Type> {
    match *t {
        True | False => Ok(Type::Bool),
        If(box ref t1, box ref t2, box ref t3) => {
            if type_of(t1) == Ok(Type::Bool) {
                let t2_type = type_of(t2)?;
                let t3_type = type_of(t3)?;

                if t2_type == t3_type {
                    Ok(t2_type)
                } else {
                    Err(Error::MismatchingArms)
                }
            } else {
                Err(Error::NonBoolCondition)
            }
        }
        Zero => Ok(Type::Nat),
        Succ(box ref t1) => {
            let t1_type = type_of(t1)?;

            if t1_type == Type::Nat {
                Ok(Type::Nat)
            } else {
                Err(Error::NonNumericSucc)
            }
        }
        Pred(box ref t1) => {
            let t1_type = type_of(t1)?;

            if t1_type == Type::Nat {
                Ok(Type::Nat)
            } else {
                Err(Error::NonNumericPred)
            }
        }
        IsZero(box ref t1) => {
            let t1_type = type_of(t1)?;

            if t1_type == Type::Nat {
                Ok(Type::Bool)
            } else {
                Err(Error::NonNumericIsZero)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_of_true_is_bool() {
        let t = type_of(&True).expect("Should not error");

        assert_eq!(Type::Bool, t);
    }

    #[test]
    fn type_of_false_is_bool() {
        let t = type_of(&False).expect("Should not error");

        assert_eq!(Type::Bool, t);
    }

    #[test]
    fn type_of_well_typed_if_is_the_type_of_branches() {
        let t = type_of(&If(box True, box Zero, box Zero)).expect("Should not error");

        assert_eq!(Type::Nat, t);
    }

    #[test]
    fn type_of_if_with_mismatching_arms_is_error() {
        let t = type_of(&If(box True, box True, box Zero));

        assert_eq!(Err(Error::MismatchingArms), t);
    }

    #[test]
    fn type_of_if_with_nonboolean_condition_is_error() {
        let t = type_of(&If(box Zero, box True, box True));

        assert_eq!(Err(Error::NonBoolCondition), t);
    }

    #[test]
    fn type_of_zero_is_nat() {
        let t = type_of(&Zero).expect("Should not error");

        assert_eq!(Type::Nat, t);
    }

    #[test]
    fn type_of_succ_zero_is_nat() {
        let t = type_of(&Succ(box Zero)).expect("Should not error");

        assert_eq!(Type::Nat, t);
    }

    #[test]
    fn type_of_succ_true_is_error() {
        let t = type_of(&Succ(box True));

        assert_eq!(Err(Error::NonNumericSucc), t);
    }

    #[test]
    fn type_of_pred_zero_is_nat() {
        let t = type_of(&Pred(box Zero)).expect("Should not error");

        assert_eq!(Type::Nat, t);
    }

    #[test]
    fn type_of_pred_true_is_error() {
        let t = type_of(&Pred(box True));

        assert_eq!(Err(Error::NonNumericPred), t);
    }

    #[test]
    fn type_of_iszero_is_bool() {
        let t = type_of(&IsZero(box Zero)).expect("Should not error");

        assert_eq!(Type::Bool, t);
    }

    #[test]
    fn type_of_badly_typed_iszero_is_error() {
        let t = type_of(&IsZero(box True));

        assert_eq!(Err(Error::NonNumericIsZero), t);
    }

    #[test]
    fn type_of_nested_bad_typed_if_is_innermost_error() {
        let t = type_of(&If(box True, box Succ(box True), box False));

        assert_eq!(Err(Error::NonNumericSucc), t);
    }

    #[test]
    fn type_of_nested_bad_succ_is_innermost_error() {
        let t = type_of(&Succ(box Pred(box True)));

        assert_eq!(Err(Error::NonNumericPred), t);
    }

    #[test]
    fn type_of_nested_bad_pred_is_innermost_error() {
        let t = type_of(&Pred(box Succ(box True)));

        assert_eq!(Err(Error::NonNumericSucc), t);
    }

    #[test]
    fn type_of_nested_bad_iszero_is_innermost_error() {
        let t = type_of(&IsZero(box Succ(box True)));

        assert_eq!(Err(Error::NonNumericSucc), t);
    }
}
