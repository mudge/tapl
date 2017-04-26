//! The typing rules for this language.

use Term;
use Term::{True, False, If, Zero, Succ, Pred, IsZero};
use Type;
use Type::{Bool, Nat};

/// Determine the type of a given `Term` returning either a `Type` or
/// nothing if the term is not well-typed.
pub fn type_of(t: Term) -> Option<Type> {
    match t {
        True | False => Some(Bool),
        If(box t1, box t2, box t3) => {
            if type_of(t1) == Some(Bool) {
                let t2_type = type_of(t2);

                if t2_type == type_of(t3) {
                    t2_type
                } else {
                    None
                }
            } else {
                None
            }
        },
        Zero => Some(Nat),
        Succ(box t1) => {
            if type_of(t1) == Some(Nat) {
                Some(Nat)
            } else {
                None
            }
        },
        Pred(box t1) => {
            if type_of(t1) == Some(Nat) {
                Some(Nat)
            } else {
                None
            }
        },
        IsZero(box t1) => {
            if type_of(t1) == Some(Nat) {
                Some(Bool)
            } else {
                None
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_of_true_is_bool() {
        let t = type_of(True).expect("Should not error");

        assert_eq!(Bool, t);
    }

    #[test]
    fn type_of_false_is_bool() {
        let t = type_of(False).expect("Should not error");

        assert_eq!(Bool, t);
    }

    #[test]
    fn type_of_well_typed_if_is_the_type_of_branches() {
        let t = type_of(If(box True, box Zero, box Zero)).expect("Should not error");

        assert_eq!(Nat, t);
    }

    #[test]
    fn type_of_badly_typed_if_is_unknown() {
        let t = type_of(If(box True, box True, box Zero));

        assert_eq!(None, t);
    }

    #[test]
    fn type_of_zero_is_nat() {
        let t = type_of(Zero).expect("Should not error");

        assert_eq!(Nat, t);
    }

    #[test]
    fn type_of_succ_zero_is_nat() {
        let t = type_of(Succ(box Zero)).expect("Should not error");

        assert_eq!(Nat, t);
    }

    #[test]
    fn type_of_succ_true_is_unknown() {
        let t = type_of(Succ(box True));

        assert_eq!(None, t);
    }

    #[test]
    fn type_of_pred_zero_is_nat() {
        let t = type_of(Pred(box Zero)).expect("Should not error");

        assert_eq!(Nat, t);
    }

    #[test]
    fn type_of_pred_true_is_unknown() {
        let t = type_of(Pred(box True));

        assert_eq!(None, t);
    }

    #[test]
    fn type_of_iszero_is_bool() {
        let t = type_of(IsZero(box Zero)).expect("Should not error");

        assert_eq!(Bool, t);
    }

    #[test]
    fn type_of_badly_typed_iszero_is_unknown() {
        let t = type_of(IsZero(box True));

        assert_eq!(None, t);
    }
}
