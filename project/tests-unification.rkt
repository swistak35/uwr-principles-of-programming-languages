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
      "var and var"
      (runner
        (an-equality (var-type 1) (var-type 2)))
      `((1 ,(var-type 2))))

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

    (test-equal?
      "var and arrow(int, int)"
      (runner
        (an-equality (var-type 1) (arrow-type (int-type) (int-type))))
      `((1 ,(arrow-type (int-type) (int-type)))))

    (test-equal?
      "arrow(int, int) and var"
      (runner
        (an-equality (arrow-type (int-type) (int-type)) (var-type 1)))
      `((1 ,(arrow-type (int-type) (int-type)))))

    (test-exn
      "bool and arrow(int, int)"
      #rx"Unification"
      (runner-d
        (an-equality (bool-type) (arrow-type (int-type) (int-type)))))

    (test-exn
      "int and arrow(int, int)"
      #rx"Unification"
      (runner-d
        (an-equality (int-type) (arrow-type (int-type) (int-type)))))

    (test-exn
      "arrow(int, int) and bool"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (int-type) (int-type)) (bool-type))))

    (test-exn
      "arrow(int, int) and int"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (int-type) (int-type)) (int-type))))

    (test-equal?
      "arrow(int, int) and arrow(int,int)"
      (runner
        (an-equality (arrow-type (int-type) (int-type)) (arrow-type (int-type) (int-type))))
      `())

    (test-exn
      "arrow(bool, int) and arrow(int,int)"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (bool-type) (int-type)) (arrow-type (int-type) (int-type)))))

    (test-exn
      "arrow(int, int) and arrow(bool, int)"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (int-type) (int-type)) (arrow-type (bool-type) (int-type)))))

    (test-exn
      "arrow(int, bool) and arrow(int, int)"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (int-type) (bool-type)) (arrow-type (int-type) (int-type)))))

    (test-exn
      "arrow(int, int) and arrow(int, bool)"
      #rx"Unification"
      (runner-d
        (an-equality (arrow-type (int-type) (int-type)) (arrow-type (int-type) (bool-type)))))

    (test-equal?
      "arrow(int, bool) and arrow(int, bool)"
      (runner
        (an-equality (arrow-type (int-type) (bool-type)) (arrow-type (int-type) (bool-type))))
      `())

    (test-equal?
      "arrow(var, int) and arrow(int, int)"
      (runner
        (an-equality (arrow-type (var-type 1) (bool-type)) (arrow-type (int-type) (bool-type))))
      `(
        (1 ,(int-type))))

    (test-equal?
      "int list and var"
      (runner
        (an-equality (list-type (int-type)) (var-type 1)))
      `(
        (1 ,(list-type (int-type)))))

    (test-equal?
      "var and int list"
      (runner
        (an-equality (var-type 1) (list-type (int-type))))
      `(
        (1 ,(list-type (int-type)))))

    (test-equal?
      "var list and int list"
      (runner
        (an-equality (list-type (var-type 1)) (list-type (int-type))))
      `(
        (1 ,(int-type))))

    (test-equal?
      "int list and var list"
      (runner
        (an-equality (list-type (int-type)) (list-type (var-type 1))))
      `(
        (1 ,(int-type))))

    (test-equal?
      "int list list and var list"
      (runner
        (an-equality (list-type (list-type (int-type))) (list-type (var-type 1))))
      `(
        (1 ,(list-type (int-type)))))

  ))
