#[derive(PartialEq, Debug)]
enum Error {
    NoRuleApplies,
}

#[derive(PartialEq, Debug, Clone)]
enum Term {
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
}

fn eval1(t: Term) -> Result<Term, Error> {
    match t {
        Term::If(t1, t2, t3) => match *t1 {
            Term::True => Ok(*t2),
            Term::False => Ok(*t3),
            _ => Ok(Term::If(Box::new(eval1(*t1)?), t2, t3)),
        },
        Term::Succ(t1) => Ok(Term::Succ(Box::new(eval1(*t1)?))),
        Term::Pred(t1) => match *t1 {
            Term::Zero => Ok(Term::Zero),
            Term::Succ(ref nv1) if is_numeric_val(&**nv1) => Ok(*(*nv1).clone()),
            _ => Ok(Term::Pred(Box::new(eval1(*t1)?))),
        },
        Term::IsZero(t1) => match *t1 {
            Term::Zero => Ok(Term::True),
            Term::Succ(ref nv1) if is_numeric_val(&**nv1) => Ok(Term::False),
            _ => Ok(Term::IsZero(Box::new(eval1(*t1)?))),
        },
        _ => Err(Error::NoRuleApplies),
    }
}

fn is_val(t: &Term) -> bool {
    match *t {
        Term::True => true,
        Term::False => true,
        ref t if is_numeric_val(t) => true,
        _ => false,
    }
}

fn is_numeric_val(t: &Term) -> bool {
    match *t {
        Term::Zero => true,
        Term::Succ(ref t1) => is_numeric_val(&*t1),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_numeric_val_returns_true_for_zero() {
        assert!(is_numeric_val(&Term::Zero));
    }

    #[test]
    fn is_numeric_val_returns_true_for_succ_zero() {
        assert!(is_numeric_val(&Term::Succ(Box::new(Term::Zero))));
    }

    #[test]
    fn is_numeric_val_returns_false_for_true() {
        assert!(!is_numeric_val(&Term::True));
    }

    #[test]
    fn is_numeric_val_returns_false_for_invalid_succ() {
        assert!(!is_numeric_val(&Term::Succ(Box::new(Term::False))));
    }

    #[test]
    fn is_val_returns_true_for_true() {
        assert!(is_val(&Term::True));
    }

    #[test]
    fn is_val_returns_true_for_false() {
        assert!(is_val(&Term::False));
    }

    #[test]
    fn is_val_returns_true_for_zero() {
        assert!(is_val(&Term::Zero));
    }

    #[test]
    fn is_val_returns_true_for_succ_zero() {
        assert!(is_val(&Term::Succ(Box::new(Term::Zero))));
    }

    #[test]
    fn is_val_returns_false_for_conditionals() {
        assert!(!is_val(&Term::If(Box::new(Term::True), Box::new(Term::True), Box::new(Term::True))));
    }

    #[test]
    fn eval1_evaluates_if_true_to_consequent() {
        let if_true = Term::If(Box::new(Term::True), Box::new(Term::True), Box::new(Term::False));
        let evaluation = eval1(if_true).expect("Should not error");

        assert_eq!(Term::True, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_false_to_alternate() {
        let if_false = Term::If(Box::new(Term::False), Box::new(Term::True), Box::new(Term::False));
        let evaluation = eval1(if_false).expect("Should not error");

        assert_eq!(Term::False, evaluation);
    }

    #[test]
    fn eval1_evaluates_if_iszero_zero_to_if_true() {
        let if_iszero_zero = Term::If(Box::new(Term::IsZero(Box::new(Term::Zero))), Box::new(Term::True), Box::new(Term::False));
        let evaluation = eval1(if_iszero_zero).expect("Should not error");

        assert_eq!(Term::If(Box::new(Term::True), Box::new(Term::True), Box::new(Term::False)), evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_zero_to_true() {
        let iszero_zero = Term::IsZero(Box::new(Term::Zero));
        let evaluation = eval1(iszero_zero).expect("Should not error");

        assert_eq!(Term::True, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_succ_to_false() {
        let iszero_succ = Term::IsZero(Box::new(Term::Succ(Box::new(Term::Zero))));
        let evaluation = eval1(iszero_succ).expect("Should not error");

        assert_eq!(Term::False, evaluation);
    }

    #[test]
    fn eval1_evaluates_iszero_pred_zero_to_iszero_zero() {
        let iszero_pred_zero = Term::IsZero(Box::new(Term::Pred(Box::new(Term::Zero))));
        let evaluation = eval1(iszero_pred_zero).expect("Should not error");

        assert_eq!(Term::IsZero(Box::new(Term::Zero)), evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_zero_to_zero() {
        let pred_zero = Term::Pred(Box::new(Term::Zero));
        let evaluation = eval1(pred_zero).expect("Should not error");

        assert_eq!(Term::Zero, evaluation);
    }

    #[test]
    fn eval1_evaluates_pred_succ_zero_to_zero() {
        let pred_succ_zero = Term::Pred(Box::new(Term::Succ(Box::new(Term::Zero))));
        let evaluation = eval1(pred_succ_zero).expect("Should not error");

        assert_eq!(Term::Zero, evaluation);
    }
}
