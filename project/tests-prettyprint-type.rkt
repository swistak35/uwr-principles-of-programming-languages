#lang eopl

(require rackunit)
(require "type-data-structures.rkt")
(require "prettyprint-type.rkt")

(provide tests-prettyprint-type)

(define (runner type-annotation)
  (prettyprint-type type-annotation))

(define (my-test-equal? val1 val2)
  (test-equal? val2 val1 val2))

(define tests-prettyprint-type
  (test-suite
    "Type prettyprint"

    (my-test-equal?
      (runner (int-type))
      "int")

    (my-test-equal?
      (runner (bool-type))
      "bool")

    (my-test-equal?
      (runner (arrow-type (int-type) (bool-type)))
      "int -> bool")

    (my-test-equal?
      (runner (arrow-type (arrow-type (int-type) (bool-type)) (int-type)))
      "(int -> bool) -> int")

    (my-test-equal?
      (runner (arrow-type (int-type) (arrow-type (int-type) (bool-type))))
      "int -> int -> bool")

    (my-test-equal?
      (runner (list-type (int-type)))
      "int list")

    (my-test-equal?
      (runner (list-type (var-type 1)))
      "'1 list")

    (my-test-equal?
      (runner (var-type 1))
      "'1")

    (my-test-equal?
      (runner (ref-type (int-type)))
      "int ref")

    (my-test-equal?
      (runner (ref-type (ref-type (int-type))))
      "int ref ref")

    (my-test-equal?
      (runner (arrow-type (ref-type (int-type)) (list-type (bool-type))))
      "int ref -> bool list")

    (my-test-equal?
      (runner (tuple-type (list (int-type) (bool-type))))
      "int * bool")

    (my-test-equal?
      (runner (tuple-type (list (int-type) (bool-type) (int-type))))
      "int * bool * int")

    (my-test-equal?
      (runner (tuple-type (list (int-type) (tuple-type (list (bool-type) (bool-type))) (int-type))))
      "int * (bool * bool) * int")

    ))
