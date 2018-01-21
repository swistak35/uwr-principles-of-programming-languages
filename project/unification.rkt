#lang eopl

(require "type-data-structures.rkt")

(provide (all-defined-out))

(define-datatype assumption-set assumption-set?
  (empty-aset)
  (extend-aset 
    (bvar symbol?)
    (btype type?)
    (saved-aset assumption-set?)))

(define (apply-aset aset var)
  (cases assumption-set aset
    (empty-aset ()
      (eopl:error 'apply-aset "No binding for ~s" var))
    (extend-aset (bvar btype saved-aset)
      (if (eqv? var bvar)
        btype
        (apply-aset saved-aset var)))))

(define-datatype substitution substitution?
  (empty-subst)
  (extend-subst 
    (varid integer?)
    (stype type?)
    (saved-subst substitution?)))

(define (apply-subst subst search-id)
  (cases substitution subst
    (empty-subst ()
      (var-type search-id))
    (extend-subst (id stype saved-subst)
      (if (eqv? id search-id)
        stype
        (apply-subst saved-subst search-id)))))

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


(define (unify equalities subst)
  (if (null? equalities)
    subst
    (let* ((cur-eq (car equalities))
           (cur-eq-left (eq->left cur-eq))
           (cur-eq-right (eq->right cur-eq)))
      (cases type cur-eq-left
        (var-type (id1)
          (cases type cur-eq-right
            (int-type ()
              (unify (cdr equalities) (extend-subst id1 (int-type) subst)))
            (else
              (if (equal? cur-eq-left cur-eq-right)
                (unify (cdr equalities) subst)
                (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))
        (int-type ()
          (cases type cur-eq-right
            (var-type (id1)
              (unify (cdr equalities) (extend-subst id1 (int-type) subst)))
            (else
              (if (equal? cur-eq-left cur-eq-right)
                (unify (cdr equalities) subst)
                (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))
        (else
          (if (equal? cur-eq-left cur-eq-right)
            (unify (cdr equalities) subst)
            (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))))

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
    (else eopl:error 'subst-in-type "Unhandled type ~s" stype)))

