#lang eopl

(require rackunit)
(require "type-data-structures.rkt")
(require "unification.rkt")

(provide unification-tests)

(define (subst->list subst)
  (cases substitution subst
    (empty-subst ()
      '())
    (extend-subst (id stype saved-subst)
      (cons (list id stype) (subst->list saved-subst)))))

(define (runner . equalities)
  (subst->list (unify equalities (empty-subst))))

(define (runner-d . equalities)
  (lambda () (apply runner equalities)))

(define unification-tests
  (test-suite
    "Unification"

    (test-equal?
      "int and int"
      (runner
        (an-equality (int-type) (int-type)))
      `())

    (test-equal?
      "var and int"
      (runner
        (an-equality (var-type 1) (int-type)))
      `((1 ,(int-type))))

    (test-equal?
      "int and var"
      (runner
        (an-equality (int-type) (var-type 1)))
      `((1 ,(int-type))))

    (test-equal?
      "bool and bool"
      (runner
        (an-equality (bool-type) (bool-type)))
      `())

    (test-equal?
      "var and bool"
      (runner
        (an-equality (var-type 1) (bool-type)))
      `((1 ,(bool-type))))

    (test-equal?
      "bool and var"
      (runner
        (an-equality (bool-type) (var-type 1)))
      `((1 ,(bool-type))))

    (test-exn
      "bool and int"
      #rx"Unification"
      (runner-d
        (an-equality (bool-type) (int-type))))

    (test-exn
      "int and bool"
      #rx"Unification"
      (runner-d
        (an-equality (int-type) (bool-type))))

  ))
