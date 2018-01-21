#lang eopl

(require rackunit)
(require "lang.scm")
(require "type-inference.scm")

(provide inference-tests)

(define (runner prg)
  (infer/pgm (scan&parse prg)))

(define (runner-d prg)
  (lambda () (runner prg)))

(define inference-tests
  (test-suite
    "Type inference"

    (test-equal?
      "Positive constants"
      (runner "12")
      (int-type))

    (test-equal?
      "zero? with int"
      (runner "zero?(42)")
      (bool-type))

    (test-exn
      "zero? with bool"
      #rx"Unification"
      (runner-d "zero?(zero?(42))"))

    (test-equal?
      "if correct"
      (runner "if zero?(42) then 1 else 2")
      (int-type))

    (test-exn
      "if with incorrect condition type"
      #rx"Unification"
      (runner-d "if 42 then 1 else 2"))

    (test-exn
      "if with different result types"
      #rx"Unification"
      (runner-d "if zero?(42) then zero?(1) else 2"))

    (test-equal?
      "diff correct"
      (runner "-(42, 22)")
      (int-type))

    (test-exn
      "diff incorrect 1st arg"
      #rx"Unification"
      (runner-d "-(zero?(42), 22)"))

    (test-exn
      "diff incorrect 2nd arg"
      #rx"Unification"
      (runner-d "-(22, zero?(42))"))

    (test-equal?
      "simple proc"
      (runner "proc(y) -(42, y)")
      (arrow-type (int-type) (int-type)))

    (test-equal?
      "simple proc 2"
      (runner "proc(y) zero?(y)")
      (arrow-type (int-type) (bool-type)))

    (test-equal?
      "simple polymorphic proc"
      (runner "proc(y) 42")
      (arrow-type (var-type 1) (int-type)))

    ))
