#lang eopl

(require "type-data-structures.rkt")

; for prettyprinter
(require (only-in racket/base
                  format))
(require (only-in racket/string
                  string-join))
(require "prettyprint-type.rkt")

(provide (all-defined-out))

(define-datatype substitution substitution?
  (empty-subst)
  (extend-subst 
    (varid integer?)
    (stype type?)
    (saved-subst substitution?)))

(define (extend-subst! varid stype saved-subst)
  (let ((search-result (find-subst saved-subst varid)))
    (if (and search-result (not (equal? search-result stype)))
      (eopl:error 'extend-subst! "Substitution ~s to ~s can't be added because already present ~s" varid stype search-result)
      (extend-subst varid stype saved-subst))))

(define (merge-subst subst1 subst2)
  (dumb-merge-subst
    (subst-in-substs subst2 subst1)
    subst2))

(define (dumb-merge-subst subst1 subst2)
  (cases substitution subst2 ; it's a weird (reverse) order, but it shouldn't matter
    (empty-subst () subst1)
    (extend-subst (id stype saved-subst)
      (dumb-merge-subst (extend-subst! id stype subst1) saved-subst))))

(define (find-subst subst search-id)
  (cases substitution subst
    (empty-subst () #f)
    (extend-subst (id stype saved-subst)
      (if (eqv? id search-id)
        stype
        (find-subst saved-subst search-id)))))

(define (apply-subst subst search-id)
  (let ((search-result (find-subst subst search-id)))
    (if search-result
      search-result
      (var-type search-id))))

(define-datatype equality equality?
  (an-equality
    (left type?)
    (right type?)))

(define (eq->left eq)
  (cases equality eq
    (an-equality (eq-left eq-right) eq-left)))

(define (eq->right eq)
  (cases equality eq
    (an-equality (eq-left eq-right) eq-right)))

(define (subst-in-equalities subst equalities)
  (map
    (lambda (eq)
      (an-equality
        (subst-in-type subst (eq->left eq))
        (subst-in-type subst (eq->right eq))))
    equalities))

(define (unify/var-sth equalities saved-subst id sth)
  (let ((new-subst (merge-subst saved-subst (extend-subst id sth (empty-subst)))))
    (unify (subst-in-equalities new-subst equalities) new-subst)))

(define (unify/arrow-arrow equalities saved-subst left1 right1 left2 right2)
  (unify
    (append
      (list
        (an-equality left1 left2)
        (an-equality right1 right2))
      equalities)
    saved-subst))

(define (unify/list-list equalities saved-subst elem1 elem2)
  (unify
    (cons (an-equality elem1 elem2) equalities) saved-subst))

(define (unify/ref-ref equalities saved-subst elem1 elem2)
  (unify/list-list equalities saved-subst elem1 elem2))

(define (unify/tuple-tuple equalities saved-subst elems1 elems2)
  (unify
    (append
      (map (lambda (elem1 elem2) (an-equality elem1 elem2)) elems1 elems2)
      equalities)
    saved-subst))

(define (unify/simple equalities saved-subst left right)
  (if (equal? left right)
    (unify equalities saved-subst)
    (eopl:error 'infer "Unification of ~s with ~s failed with substitution ~s" left right saved-subst)))

(define (unify equalities subst)
  (if (null? equalities)
    subst
    (let* ((cur-eq (car equalities))
           (cur-eq-left (eq->left cur-eq))
           (cur-eq-right (eq->right cur-eq)))
      (cases type cur-eq-left
        (var-type (id1)
          (unify/var-sth (cdr equalities) subst id1 cur-eq-right))
        (int-type ()
          (cases type cur-eq-right
            (var-type (id1)
              (unify/var-sth (cdr equalities) subst id1 cur-eq-left))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        (bool-type ()
          (cases type cur-eq-right
            (var-type (id1)
              (unify/var-sth (cdr equalities) subst id1 cur-eq-left))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        (arrow-type (left right)
          (cases type cur-eq-right
            (var-type (id1)
              (unify/var-sth (cdr equalities) subst id1 cur-eq-left))
            (arrow-type (left2 right2)
              (unify/arrow-arrow (cdr equalities) subst left right left2 right2))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        (list-type (elem1)
          (cases type cur-eq-right
            (var-type (id1)
              (unify/var-sth (cdr equalities) subst id1 cur-eq-left))
            (list-type (elem2)
              (unify/list-list (cdr equalities) subst elem1 elem2))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        (tuple-type (elems1)
          (cases type cur-eq-right
            (var-type (id2)
              (unify/var-sth (cdr equalities) subst id2 cur-eq-left))
            (tuple-type (elems2)
              (unify/tuple-tuple (cdr equalities) subst elems1 elems2))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        (ref-type (elem1)
          (cases type cur-eq-right
            (var-type (id1)
              (unify/var-sth (cdr equalities) subst id1 cur-eq-left))
            (ref-type (elem2)
              (unify/ref-ref (cdr equalities) subst elem1 elem2))
            (else (unify/simple (cdr equalities) subst cur-eq-left cur-eq-right))))
        ))))

(define (subst-in-type subst stype)
  (cases type stype
    (int-type ()
      (int-type))
    (bool-type ()
      (bool-type))
    (arrow-type (left right)
      (arrow-type (subst-in-type subst left) (subst-in-type subst right)))
    (var-type (id)
      (apply-subst subst id))
    (list-type (elem)
      (list-type (subst-in-type subst elem)))
    (tuple-type (types)
      (tuple-type (map (lambda (t) (subst-in-type subst t)) types)))
    (ref-type (elem)
      (ref-type (subst-in-type subst elem)))))

(define (subst-in-substs subst substituted)
  (cases substitution substituted
    (empty-subst () (empty-subst))
    (extend-subst (id stype saved-subst)
      (extend-subst id (subst-in-type subst stype) (subst-in-substs subst saved-subst)))))

(define (prettyprint-subst subst)
  (format
    "{~a}"
    (string-join
      (map
        (lambda (subst-elem) (format "~a/~a" (car subst-elem) (prettyprint-type (cadr subst-elem))))
        (subst->list subst))
      "; ")))

(define (subst->list subst)
  (cases substitution subst
    (empty-subst () '())
    (extend-subst (id stype saved-subst)
      (cons (list id stype) (subst->list saved-subst)))))

; (require trace)
; (trace unify)
; (trace merge-subst)
