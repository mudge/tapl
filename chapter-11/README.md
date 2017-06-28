# Chapter 11: Simple Extensions

An implementation in Rust of the [`fullsimple`](https://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullsimple/core.ml) syntax and evaluation rules.

Note this currently requires nightly Rust in order to use [Box syntax and patterns](https://doc.rust-lang.org/book/box-syntax-and-patterns.html).

```
Source term:    case inl {true,true} as Bool×Bool+Unit of inl p ⇒ p.1 | inr u ⇒ false
Resulting type: Bool
```
