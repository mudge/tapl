use Term;
extern crate lalrpop_util as __lalrpop_util;

mod __parse__Term {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use Term;
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(dead_code)]
    pub enum __Symbol<'input> {
        Term_220_22(&'input str),
        Term_22else_22(&'input str),
        Term_22false_22(&'input str),
        Term_22if_22(&'input str),
        Term_22iszero_22(&'input str),
        Term_22pred_22(&'input str),
        Term_22succ_22(&'input str),
        Term_22then_22(&'input str),
        Term_22true_22(&'input str),
        NtTerm(Term),
        Nt____Term(Term),
    }
    const __ACTION: &'static [i32] = &[// State 0
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 1
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       -8,
                                       // State 2
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       -7,
                                       // State 3
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       -6,
                                       // State 4
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 5
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 6
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 7
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 8
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       -5,
                                       // State 9
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       14,
                                       0,
                                       // State 10
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       -4,
                                       // State 11
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       -3,
                                       // State 12
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       -2,
                                       // State 13
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 14
                                       0,
                                       16,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       // State 15
                                       3,
                                       0,
                                       4,
                                       5,
                                       6,
                                       7,
                                       8,
                                       0,
                                       9,
                                       // State 16
                                       -1,
                                       -1,
                                       -1,
                                       -1,
                                       -1,
                                       -1,
                                       -1,
                                       -1,
                                       -1];
    const __EOF_ACTION: &'static [i32] = &[0, -8, -7, -6, 0, 0, 0, 0, -5, 0, -4, -3, -2, 0, 0, 0,
                                           -1];
    const __GOTO: &'static [i32] = &[// State 0
                                     2,
                                     0,
                                     // State 1
                                     0,
                                     0,
                                     // State 2
                                     0,
                                     0,
                                     // State 3
                                     0,
                                     0,
                                     // State 4
                                     10,
                                     0,
                                     // State 5
                                     11,
                                     0,
                                     // State 6
                                     12,
                                     0,
                                     // State 7
                                     13,
                                     0,
                                     // State 8
                                     0,
                                     0,
                                     // State 9
                                     0,
                                     0,
                                     // State 10
                                     0,
                                     0,
                                     // State 11
                                     0,
                                     0,
                                     // State 12
                                     0,
                                     0,
                                     // State 13
                                     15,
                                     0,
                                     // State 14
                                     0,
                                     0,
                                     // State 15
                                     17,
                                     0,
                                     // State 16
                                     0,
                                     0];
    fn __expected_tokens(__state: usize) -> Vec<::std::string::String> {
        const __TERMINAL: &'static [&'static str] = &[r###""0""###,
                                                      r###""else""###,
                                                      r###""false""###,
                                                      r###""if""###,
                                                      r###""iszero""###,
                                                      r###""pred""###,
                                                      r###""succ""###,
                                                      r###""then""###,
                                                      r###""true""###];
        __ACTION[(__state * 9)..]
            .iter()
            .zip(__TERMINAL)
            .filter_map(|(&state, terminal)| {
                if state == 0 {
                    None
                } else {
                    Some(terminal.to_string())
                }
            })
            .collect()
    }
    pub fn parse_Term<'input>
        (input: &'input str)
         -> Result<Term, __lalrpop_util::ParseError<usize, (usize, &'input str), ()>> {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let mut __states = vec![0_i32];
        let mut __symbols = vec![];
        let mut __integer;
        let mut __lookahead;
        let mut __last_location = Default::default();
        '__shift: loop {
            __lookahead = match __tokens.next() {
                Some(Ok(v)) => v,
                None => break '__shift,
                Some(Err(e)) => return Err(e),
            };
            __last_location = __lookahead.2.clone();
            __integer = match __lookahead.1 {
                (0, _) if true => 0,
                (1, _) if true => 1,
                (2, _) if true => 2,
                (3, _) if true => 3,
                (4, _) if true => 4,
                (5, _) if true => 5,
                (6, _) if true => 6,
                (7, _) if true => 7,
                (8, _) if true => 8,
                _ => {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error);
                }
            };
            '__inner: loop {
                let __state = *__states.last().unwrap() as usize;
                let __action = __ACTION[__state * 9 + __integer];
                if __action > 0 {
                    let __symbol = match __integer {
                        0 => {
                            match __lookahead.1 {
                                (0, __tok0) => __Symbol::Term_220_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        1 => {
                            match __lookahead.1 {
                                (1, __tok0) => __Symbol::Term_22else_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        2 => {
                            match __lookahead.1 {
                                (2, __tok0) => __Symbol::Term_22false_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        3 => {
                            match __lookahead.1 {
                                (3, __tok0) => __Symbol::Term_22if_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        4 => {
                            match __lookahead.1 {
                                (4, __tok0) => __Symbol::Term_22iszero_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        5 => {
                            match __lookahead.1 {
                                (5, __tok0) => __Symbol::Term_22pred_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        6 => {
                            match __lookahead.1 {
                                (6, __tok0) => __Symbol::Term_22succ_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        7 => {
                            match __lookahead.1 {
                                (7, __tok0) => __Symbol::Term_22then_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        8 => {
                            match __lookahead.1 {
                                (8, __tok0) => __Symbol::Term_22true_22((__tok0)),
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };
                    __states.push(__action - 1);
                    __symbols.push((__lookahead.0, __symbol, __lookahead.2));
                    continue '__shift;
                } else if __action < 0 {
                    if let Some(r) = __reduce(input,
                                              __action,
                                              Some(&__lookahead.0),
                                              &mut __states,
                                              &mut __symbols,
                                              ::std::marker::PhantomData::<()>) {
                        return r;
                    }
                } else {
                    let __state = *__states.last().unwrap() as usize;
                    let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: __expected_tokens(__state),
                    };
                    return Err(__error);
                }
            }
        }
        loop {
            let __state = *__states.last().unwrap() as usize;
            let __action = __EOF_ACTION[__state];
            if __action < 0 {
                if let Some(r) = __reduce(input,
                                          __action,
                                          None,
                                          &mut __states,
                                          &mut __symbols,
                                          ::std::marker::PhantomData::<()>) {
                    return r;
                }
            } else {
                let __state = *__states.last().unwrap() as usize;
                let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                    token: None,
                    expected: __expected_tokens(__state),
                };
                return Err(__error);
            }
        }
    }
    pub fn __reduce<'input>
        (input: &'input str,
         __action: i32,
         __lookahead_start: Option<&usize>,
         __states: &mut ::std::vec::Vec<i32>,
         __symbols: &mut ::std::vec::Vec<(usize, __Symbol<'input>, usize)>,
         _: ::std::marker::PhantomData<()>)
         -> Option<Result<Term, __lalrpop_util::ParseError<usize, (usize, &'input str), ()>>> {
        let __nonterminal = match -__action {
            1 => {
                // Term = "if", Term, "then", Term, "else", Term => ActionFn(1);
                let __sym5 = __pop_NtTerm(__symbols);
                let __sym4 = __pop_Term_22else_22(__symbols);
                let __sym3 = __pop_NtTerm(__symbols);
                let __sym2 = __pop_Term_22then_22(__symbols);
                let __sym1 = __pop_NtTerm(__symbols);
                let __sym0 = __pop_Term_22if_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym5.2.clone();
                let __nt = super::__action1(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
                let __states_len = __states.len();
                __states.truncate(__states_len - 6);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            2 => {
                // Term = "succ", Term => ActionFn(2);
                let __sym1 = __pop_NtTerm(__symbols);
                let __sym0 = __pop_Term_22succ_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action2(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            3 => {
                // Term = "pred", Term => ActionFn(3);
                let __sym1 = __pop_NtTerm(__symbols);
                let __sym0 = __pop_Term_22pred_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action3(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            4 => {
                // Term = "iszero", Term => ActionFn(4);
                let __sym1 = __pop_NtTerm(__symbols);
                let __sym0 = __pop_Term_22iszero_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action4(input, __sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            5 => {
                // Term = "true" => ActionFn(5);
                let __sym0 = __pop_Term_22true_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            6 => {
                // Term = "false" => ActionFn(6);
                let __sym0 = __pop_Term_22false_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action6(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            7 => {
                // Term = "0" => ActionFn(7);
                let __sym0 = __pop_Term_220_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7(input, __sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtTerm(__nt), __end));
                0
            }
            8 => {
                // __Term = Term => ActionFn(0);
                let __sym0 = __pop_NtTerm(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0(input, __sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action),
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 2 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_220_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                    __Symbol<'input>,
                                                                    usize)>)
                                 -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_220_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22else_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                       __Symbol<'input>,
                                                                       usize)>)
                                    -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22else_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22false_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                        __Symbol<'input>,
                                                                        usize)>)
                                     -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22false_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22if_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                     __Symbol<'input>,
                                                                     usize)>)
                                  -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22if_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22iszero_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                         __Symbol<'input>,
                                                                         usize)>)
                                      -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22iszero_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22pred_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                       __Symbol<'input>,
                                                                       usize)>)
                                    -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22pred_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22succ_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                       __Symbol<'input>,
                                                                       usize)>)
                                    -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22succ_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22then_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                       __Symbol<'input>,
                                                                       usize)>)
                                    -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22then_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Term_22true_22<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                       __Symbol<'input>,
                                                                       usize)>)
                                    -> (usize, &'input str, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22true_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_NtTerm<'input>(__symbols: &mut ::std::vec::Vec<(usize, __Symbol<'input>, usize)>)
                            -> (usize, Term, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtTerm(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
    fn __pop_Nt____Term<'input>(__symbols: &mut ::std::vec::Vec<(usize,
                                                                   __Symbol<'input>,
                                                                   usize)>)
                                -> (usize, Term, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Term(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch"),
        }
    }
}
pub use self::__parse__Term::parse_Term;
mod __intern_token {
    #![allow(unused_imports)]
    use Term;
    extern crate lalrpop_util as __lalrpop_util;
    extern crate regex as __regex;
    pub struct __Matcher<'input> {
        text: &'input str,
        consumed: usize,
        regex_set: __regex::RegexSet,
        regex_vec: Vec<__regex::Regex>,
    }

    impl<'input> __Matcher<'input> {
        pub fn new(s: &'input str) -> __Matcher<'input> {
            let __strs: &[&str] = &["^(?u:0)",
                                    "^(?u:else)",
                                    "^(?u:false)",
                                    "^(?u:if)",
                                    "^(?u:iszero)",
                                    "^(?u:pred)",
                                    "^(?u:succ)",
                                    "^(?u:then)",
                                    "^(?u:true)"];
            let __regex_set = __regex::RegexSet::new(__strs).unwrap();
            let __regex_vec = vec![__regex::Regex::new("^(?u:0)").unwrap(),
                                   __regex::Regex::new("^(?u:else)").unwrap(),
                                   __regex::Regex::new("^(?u:false)").unwrap(),
                                   __regex::Regex::new("^(?u:if)").unwrap(),
                                   __regex::Regex::new("^(?u:iszero)").unwrap(),
                                   __regex::Regex::new("^(?u:pred)").unwrap(),
                                   __regex::Regex::new("^(?u:succ)").unwrap(),
                                   __regex::Regex::new("^(?u:then)").unwrap(),
                                   __regex::Regex::new("^(?u:true)").unwrap()];
            __Matcher {
                text: s,
                consumed: 0,
                regex_set: __regex_set,
                regex_vec: __regex_vec,
            }
        }
    }

    impl<'input> Iterator for __Matcher<'input> {
        type Item = Result<(usize, (usize, &'input str), usize),
               __lalrpop_util::ParseError<usize, (usize, &'input str), ()>>;

        fn next(&mut self) -> Option<Self::Item> {
            let __text = self.text.trim_left();
            let __whitespace = self.text.len() - __text.len();
            let __start_offset = self.consumed + __whitespace;
            if __text.is_empty() {
                self.text = __text;
                self.consumed = __start_offset;
                None
            } else {
                let __matches = self.regex_set.matches(__text);
                if !__matches.matched_any() {
                    Some(Err(__lalrpop_util::ParseError::InvalidToken { location: __start_offset }))
                } else {
                    let mut __longest_match = 0;
                    let mut __index = 0;
                    for __i in 0..9 {
                        if __matches.matched(__i) {
                            let __match = self.regex_vec[__i].find(__text).unwrap();
                            let __len = __match.end();
                            if __len >= __longest_match {
                                __longest_match = __len;
                                __index = __i;
                            }
                        }
                    }
                    let __result = &__text[..__longest_match];
                    let __remaining = &__text[__longest_match..];
                    let __end_offset = __start_offset + __longest_match;
                    self.text = __remaining;
                    self.consumed = __end_offset;
                    Some(Ok((__start_offset, (__index, __result), __end_offset)))
                }
            }
        }
    }
}

#[allow(unused_variables)]
fn __action0<'input>(input: &'input str, (_, __0, _): (usize, Term, usize)) -> Term {
    (__0)
}

#[allow(unused_variables)]
fn __action1<'input>(input: &'input str,
                     (_, _, _): (usize, &'input str, usize),
                     (_, t1, _): (usize, Term, usize),
                     (_, _, _): (usize, &'input str, usize),
                     (_, t2, _): (usize, Term, usize),
                     (_, _, _): (usize, &'input str, usize),
                     (_, t3, _): (usize, Term, usize))
                     -> Term {
    Term::If(Box::new(t1), Box::new(t2), Box::new(t3))
}

#[allow(unused_variables)]
fn __action2<'input>(input: &'input str,
                     (_, _, _): (usize, &'input str, usize),
                     (_, __0, _): (usize, Term, usize))
                     -> Term {
    Term::Succ(Box::new(__0))
}

#[allow(unused_variables)]
fn __action3<'input>(input: &'input str,
                     (_, _, _): (usize, &'input str, usize),
                     (_, __0, _): (usize, Term, usize))
                     -> Term {
    Term::Pred(Box::new(__0))
}

#[allow(unused_variables)]
fn __action4<'input>(input: &'input str,
                     (_, _, _): (usize, &'input str, usize),
                     (_, __0, _): (usize, Term, usize))
                     -> Term {
    Term::IsZero(Box::new(__0))
}

#[allow(unused_variables)]
fn __action5<'input>(input: &'input str, (_, __0, _): (usize, &'input str, usize)) -> Term {
    Term::True
}

#[allow(unused_variables)]
fn __action6<'input>(input: &'input str, (_, __0, _): (usize, &'input str, usize)) -> Term {
    Term::False
}

#[allow(unused_variables)]
fn __action7<'input>(input: &'input str, (_, __0, _): (usize, &'input str, usize)) -> Term {
    Term::Zero
}

pub trait __ToTriple<'input> {
    type Error;
    fn to_triple(value: Self) -> Result<(usize, (usize, &'input str), usize), Self::Error>;
}

impl<'input> __ToTriple<'input> for (usize, (usize, &'input str), usize) {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize, (usize, &'input str), usize), ()> {
        Ok(value)
    }
}
impl<'input> __ToTriple<'input> for Result<(usize, (usize, &'input str), usize), ()> {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize, (usize, &'input str), usize), ()> {
        value
    }
}
