#lang eopl

(require rackunit)
(require "lang.scm")
(require "type-data-structures.rkt")
(require "type-inference.scm")
(require "type-prettyprint.rkt")

(provide inference-tests)

(define (runner prg)
  (prettyprint-type (infer/pgm (scan&parse prg))))

(define (runner-d prg)
  (lambda () (runner prg)))

(define inference-tests
  (test-suite
    "Type inference"

    ;; Basic syntax

    (test-equal?
      "Positive constants"
      (runner "12")
      "int")

    (test-equal?
      "if correct"
      (runner "if (zero? 42) then 1 else 2")
      "int")

    (test-exn
      "if with incorrect condition type"
      #rx"Unification"
      (runner-d "if 42 then 1 else 2"))

    (test-exn
      "if with different result types"
      #rx"Unification"
      (runner-d "if (zero? 42) then (zero? 1) else 2"))

    ; NOTSURE: Whether this should behave in this way?
    (test-exn
      "unbound variable"
      #rx"No binding"
      (runner-d "x"))

    ;;; Primitives

    (test-equal?
      "primitive diff"
      (runner "diff")
      "int * int -> int")

    (test-equal?
      "primitive zero?"
      (runner "zero?")
      "int -> bool")

    ; (test-equal?
    ;   "primitive newref"
    ;   (runner "newref")
    ;   "'1 -> '1 ref")

    ; (test-equal?
    ;   "primitive deref"
    ;   (runner "deref")
    ;   "'1 ref -> '1")

    ; (test-equal?
    ;   "primitive setref"
    ;   (runner "setref")
    ;   "'1 ref * '1 -> int")

    ; (test-equal?
    ;   "primitive cons"
    ;   (runner "cons")
    ;   "'1 * '1 list -> '1 list")

    ; (test-equal?
    ;   "primitive car"
    ;   (runner "car")
    ;   "'1 list -> '1")

    ; (test-equal?
    ;   "primitive cdr"
    ;   (runner "cdr")
    ;   "'1 list -> '1 list")

    ; (test-equal?
    ;   "primitive null?"
    ;   (runner "null?")
    ;   "'1 list -> bool")

    ;;; Calls

    (test-equal?
      "diff correct"
      (runner "(diff 42 22)")
      "int")

    (test-exn
      "diff incorrect 1st arg"
      #rx"Unification"
      (runner-d "(diff (zero? 42) 22)"))

    (test-exn
      "diff incorrect 2nd arg"
      #rx"Unification"
      (runner-d "(diff 22 (zero? 42))"))

    (test-equal?
      "simple proc"
      (runner "proc(y) (diff 42 y)")
      "int -> int")

    (test-equal?
      "simple proc 2"
      (runner "proc(y) (zero? y)")
      "int -> bool")

    (test-equal?
      "simple polymorphic proc"
      (runner "proc(y) 42")
      "'1 -> int")

    (test-equal?
      "zero? with int"
      (runner "(zero? 42)")
      "bool")

    (test-exn
      "zero? with bool"
      #rx"Unification"
      (runner-d "(zero? (zero? 42))"))

    (test-equal?
      "simple call"
      (runner "(proc(y) (diff 42 y) 20)")
      "int")

    (test-exn
      "call with wrong argument"
      #rx"Unification"
      (runner-d "(proc(y) (diff 42 y) (zero? 42))"))

    (test-exn
      "call to something which is not a function"
      #rx"Call to something which is not a function"
      (runner-d "(42 42)"))

    ; (test-equal?
    ;   "primitive diff"
    ;   (runner "(diff 42 20)")
    ;   (int-type))

    ))
