---
author: Iavor S. Diatchki
title: Language Oriented Programming
date: February 2019
---


# Languages

  * (XXX)
  * General purpose: complex, multiparadigm
  * DSLs: often grow into full languages
  * Micor DSL construction kit


# Language Basics

  * Expressions are "pure"
      - Evaluate to values

  * Statements are "effectful"
      - do this, then do that


# Monads

  * Languages that use statements


# Notation

  * `s : L t`
      * `s` is a statement,
      * in language `L`
      * which produces a value of type `t`.

  * Example:
      * `getchar() : C int`


# Sequencing Statements

  * If
      - `s1 : L a`
      - `s2 : L b`, with a free `x : a`
  * then
      * `{ val x = s1; s2 } : L b`
      * Or, in Haskell notation:
        ```haskell
        do x <- s1
           s2
        ```



#
