---
author: Iavor S. Diatchki
title: Language Oriented Programming
date: February 2019
---

# Thesis

  1. It is convenient to implement the sub-components of a system in
    _custom programming languages_, tailored to the needs of the component.

  2. This is the essence of "monadic" programming.

  3. What features does a host language
     need to support this style of software development?



# Language Primitives

_Expressions_ are pure:

  * evaluate to values
  * flexible evaluation order
  * example: combinatorial circuits

_Statements_ are effectful:

  * have a notion of sequencing
  * do this, then do that
  * example: a recipe


# Monads

A _monad_ is a language that uses statements.


# Notation

```Haskell
s :: L t
```

  * `s` is a statement,
  * in language `L`
  * which produces a value of type `t`.

Example:

```C
getchar() :: C int
```


# Sequencing Statements

Combine statements to form more complex ones:

If:

  * `s1 :: L a`
  * `s2 :: L b`, with a free variable `x :: a`

Then:

```Haskell
do { x <- s1; s2 } :: L b
```


# Promoting Expressions to Statements


If:
```Haskell
e :: a        -- `e` is an expression
```

Then:

```Haskell
pure e :: L a
```

In many languages this is implicit.


# Monad Laws = Reasonable Behavior

The grouping of statements is not important:

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

  * Monadic structure = bare minimum.
  * We need statements that do something.

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

# Three Questions

  1. How do we specify the features of a language?
  2. How do we write programs in a language?
  3. How do we execute programs in the language?


# Modular Language Construction

Start with a language of _primitives_, and extended with
desired _features_.

```Haskell
type MyPL =
  DeclareLanguage
    [ F3          -- Feature 3
    , F2          -- Feature 2
    , F1          -- Feature 1
    ] Prim        -- Language of primitives
```

Primitive language examples:

  * `IO`: a language for interacting with the OS
  * `Pure`: no primitive language


# Common Features

Data effects (aka variables)

  * `Val x t` adds an immutable variable
  * `Mut x t` adds a mutable variable
  * `Collector x t` adds a collector variable

Control effects

  * `Throws t`    add support for exceptions
  * `Backtracks`  add support for backtracking





# Feature Dependencies

The order in which features are added to a language is important (sometimes):

  * Data effects are _orthogonal_: order is not important.
  * Control effects are not:  order in feature list matters.

Rule:

  _Existing features take precedence over new features._


# Example

```Haskell
type PL1 =                type PL2 =
  DeclareLanguage           DeclareLanguage
    [ Throws e                [ MutVar x t
    , MutVar x t              , Throws t
    ] Pure                    ] Pure
```

How do exceptions affect changes to `x`?

  * `PL1`: changes survive exceptions
  * `PL2`: changes are rolled back on exception


# Writing Programs

* Need a common notation for similar features across multiple language
    (e.g. read a variable).

* Exact behavior is determined by the language.

```Haskell
readVal   :: HasVal x t m       => x -> m t
getMut    :: HasMut x t m       => x -> m t
setMut    :: HasMut x t m       => x -> t -> m ()
appendTo  :: HasCollector x t m => x -> t -> m ()
throw     :: Throws t m         => t -> m a
backtrack :: Backtracks m       => m a
orElse    :: Backtracks m       => m a -> m a -> m a
```

# Running Programs

Each feature can be "compiled" away:

```Haskell
val        :: Language m => (x := t) -> Val x t m a -> m a

mut        :: Language m => (x := t) -> Mut x t m a -> m (a,t)

collector  :: Language m => Col x t m a -> m (a, [t])

throws     :: Language m => Throws t m a -> m (Except t a)

backtracks :: Language m => Maybe Int -> Backtracks m a -> m [a]
```

Or, we can compile and run the whole program:
```Haskell
run :: Run m => m a -> ExeResult m a
```


# Scoped Statements

Allow for "nested" statement execution.

```Haskell
letVal    :: LetVal x t m     => x := t -> m a -> m a
collect   :: CanCollect x t m => x -> m a -> m (a, [t])
try       :: CanCatch t m     => m a -> m (Except t a)
findUpTo  :: CanSearch m      => Maybe Int -> m a -> m [a]
```

Quite useful, much trickier semantics.


# Bigger Example

A language for a type-checker:

```Haskell
type TCLang =
  [ Throws TCError              -- Critical errors
  , Val Env   (Map Name Type)   -- Types of free variables
  , Mut Subst (Map TVar Type)   -- Inferred types
  , Col Ctrs  (Set Ctr)         -- Collected constraints
  , Col Warns (Set Warn)        -- Warnings
  ] IO                          -- Interact with solvers
```


# Haskell as a Host Language

Haskell is a _great_ language for experimenting with language design:

  * Type system tracks language fragments
  * Lazyness for custom control flow operators
  * Functions for binders and modelling jumps
  * Overloading for reusable notation


# Drawbacks of Embedding in Haskell

  * Performance can be difficult to reason about
  * Embedded notation not as neat as custom syntax
  * Potentially confusing type errors
    - although custom type errors do help

# Idea

Experience with Haskell has identified a set of useful abstractions.

_Could we design a language that supports this style of programming
directly?_

