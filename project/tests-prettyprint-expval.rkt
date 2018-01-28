#lang eopl

(require rackunit)
(require "lang.rkt")
(require "data-structures.rkt")
(require "prettyprint-expval.rkt")

(provide tests-prettyprint-expval)

(define (runner val)
  (prettyprint-expval val))

(define tests-prettyprint-expval
  (test-suite
    "Expval pretty printer"

    (test-equal?
      "number"
      (runner (num-val 12))
      "12")

    (test-equal?
      "neg number"
      (runner (num-val -12))
      "-12")

    (test-equal?
      "boolean"
      (runner (bool-val #t))
      "#t")

    (test-equal?
      "boolean"
      (runner (bool-val #f))
      "#f")

    (test-equal?
      "list"
      (runner (list-val (list)))
      "[]")

    (test-equal?
      "list"
      (runner (list-val (list (num-val 42))))
      "[42]")

    (test-equal?
      "list"
      (runner (list-val (list (num-val 42) (num-val 17))))
      "[42, 17]")

    (test-equal?
      "ref"
      (runner (ref-val 42))
      "&42")

    (test-equal?
      "primitive"
      (runner (proc-val (primitive 'zero?)))
      "primitive<zero?>")

    (test-equal?
      "one-arg procedure"
      (runner (proc-val
                (procedure
                  (list 'x)
                  (call-exp (var-exp 'zero?) (list (const-exp 42)))
                  (empty-env))))
      "procedure<(x); (zero? 42)>")

    (test-equal?
      "two-arg procedure"
      (runner (proc-val
                (procedure
                  (list 'x 'y)
                  (call-exp (var-exp 'zero?) (list (const-exp 42)))
                  (empty-env))))
      "procedure<(x, y); (zero? 42)>")

    ))
