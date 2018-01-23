#lang eopl

(require (only-in "lang.scm"
                  expression?))
(require (only-in "store.scm"
                  reference?))
(require (only-in racket/base
                  format))

(provide (all-defined-out))

;;; Expressed values

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val 
    (proc proc?))
  (ref-val
    (ref reference?))
  (list-val
    (lst (list-of expval?)))
  )

(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

(define (expval->bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool v))))

(define (expval->proc v)
  (cases expval v
    (proc-val (proc) proc)
    (else (expval-extractor-error 'proc v))))

(define (expval->ref v)
  (cases expval v
    (ref-val (ref) ref)
    (else (expval-extractor-error 'reference v))))

(define (expval->list v)
  (cases expval v
    (list-val (lst) lst)
    (else (expval-extractor-error 'list v))))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value))

;;; Procedures

(define-datatype proc proc?
  (procedure
    (bvars (list-of symbol?))
    (body expression?)
    (env environment?))
  (primitive
    (name symbol?)))

;;; Environment

(define-datatype environment environment?
  (empty-env)
  (extend-env 
    (bvar symbol?)
    (bval expval?)
    (saved-env environment?))
  (extend-env-rec*
    (proc-names (list-of symbol?))
    (b-vars (list-of (list-of symbol?)))
    (proc-bodies (list-of expression?))
    (saved-env environment?)))
