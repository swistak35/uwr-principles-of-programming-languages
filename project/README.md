# Small interpreter

This is an interpreter of the language similar in syntax to ML and lisp, with type inference.
Type inference is supporting let-polymorphism but not polymorphism inside recursive (letrec) definitions.

## Basic types

* boolean (no primitive constructors)
* numbers
* homogeneous lists
* explicit references (mutable store)
* procedures (or primitives)
* tuple-type (constructed only in multiple-arg function like `(f 1 2)`, deconstructed only in definition of `letrec f(x, y) = ...`)

## Built-in primitives

Given types, I assume they are self explanatory.

* `zero? : int -> bool`
* `diff : (int * int) -> int`
* `car : '1 list -> '1`
* `cdr : '1 list -> '1 list`
* `cons : ('1 * '1 list) -> '1 list`
* `null? : '1 list -> bool`
* `newref : '1 -> '1 ref`
* `deref : '1 ref -> '1`
* `setref : ('1 ref * '1) -> int`

## Basic syntax

* numeric constants: `24`, `-24`
* lists: `[]`, `[23, 42]`, `[(zero? 0)]`
* function calls: `(null? [42])`
* multi argument function calls: `(map f [1, 2, 3])`
* let: `let x = 42 in (diff x 1)`
* lambda abstraction: `proc(x) (zero? (diff x 1))`
* letrec (allows multiple definitions and multiple arguments in functions) `letrec map(f, xs) = if (null? xs) then [] else (cons (f (car xs)) (map f (cdr xs))) in ...`
* begin...end: `begin exp1; exp2; exp3 end`

## Starting repl

`racket run-repl.rkt`

To exit, type `exit` or hit `Ctrl+D`.

## Running tests

`racket run-tests.rkt`
