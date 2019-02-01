---
author: Iavor S. Diatchki
title: Language Oriented Programming
date: February 2019
---

# Thesis

  1. When working on specific software component, it convenint to
     have a custom DSL specifically tailored to the component.

  2. This is the essence of "monadic" programming.

  3. It is interesting to consider what features does a host languge
     need to support this style of software development.





# Language Primitives

  * _Expressions_ are pure
      - evaluate to values
      - flexible evalutaion order
      - example: combinatorial circuits

  * _Statements_ are effectful
      - have a notion of sequencing
      - do this, then do that
      - example: a recipie


# Monads

A _monad_ is a language that uses statements.


# Notation

  * `s : L t`
      * `s` is a statement,
      * in language `L`
      * which produces a value of type `t`.

  * Example:
      * `getchar() : C int`


# Sequencing Statements

We can combine statements to form more complex ones:

  * If:
      - `s1 : L a`
      - `s2 : L b`, with a free variable `x : a`
  * Then:
      - `do { x <- s1; s2 } : L b`


# Promoting Expressions to Statements


  * If:
      - `e : a` (an expression)
  * Then:
      - `pure e : L a`  (a statement that just produces `e`)

  * In many languages this is implicit.

# Monad Laws = Resonable Behavior

  * The grouping of staments is not important:


```haskell
do { y <- do { x <- s1; s2 }; s3 } =
do { x <- s1; do { y <- s2; s3 } } =
do { x <- s1; y <- s2; s3 }
```
(as long as names don't get captured)

  * Expression statements don't have effects:

```haskell
do { x <- pure x; s } =
s                     =
do { x <- s; pure x }
```


# Effects

  * The monadic strucutre provides just the bare minimum.
  * The interesting part are the statements that introduce effects.

```haskell
getGreeting :: IO String
getGreeting =
  do putStrLn "What is your name?"
     x <- getLine
     pure ("Hello, " ++ x)

main :: IO ()
main =
  do msg <- getGreeting
     putStrLn msg
```

# A Menagerie of Simple Language Features




