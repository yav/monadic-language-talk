---
author: Iavor S. Diatchki
title: Language Oriented Programming
date: February 2019
---

# Thesis

  1. It is convenient to implement the sub-components of a system in
    _custom programming languages_, tailored to the needs of the component.

  2. This is the essence of "monadic" programming.

  3. What features does a host languge
     need to support this style of software development?



# Language Primitives

_Expressions_ are pure:

  * evaluate to values
  * flexible evalutaion order
  * example: combinatorial circuits

_Statements_ are effectful:

  * have a notion of sequencing
  * do this, then do that
  * example: a recipie


# Monads

A _monad_ is a language that uses statements.


# Notation

```Haskell
s : L t
```

  * `s` is a statement,
  * in language `L`
  * which produces a value of type `t`.

Example:

```C
getchar() : C int
```


# Sequencing Statements

Combine statements to form more complex ones:

If:

  * `s1 : L a`
  * `s2 : L b`, with a free variable `x : a`

Then:

```Haskell
do { x <- s1; s2 } : L b
```


# Promoting Expressions to Statements


If:
```Haskell
e : a        -- `e` is an expression
```

Then:

```Haskell
pure e : L a
```

In many languages this is implicit.


# Monad Laws = Resonable Behavior

The grouping of staments is not important:

```haskell
do { y <- do { x <- s1; s2 }; s3 } =
do { x <- s1; do { y <- s2; s3 } } =        -- modulo naming
do { x <- s1; y <- s2; s3 }
```

Expression statements don't have effects:

```haskell
do { x <- pure x; s } =
s                     =
do { x <- s; pure x }
```


# Effects

  * Monadic strucutre = bare minimum.
  * We need statemtns that do something.

Example:

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

# Classes of Effects

  * Data effects (aka "variables")
      - Read-only variables (e.g., configuration)
      - Mutable variables
      - Write-only variables (aka "collectors", e.g., logs)

  * Control effects
      - Exceptions      (early termination)
      - Backtracking    (search)
      - Continutations  (coroutines)



