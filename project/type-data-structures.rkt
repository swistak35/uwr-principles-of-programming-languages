#lang eopl

(require (only-in racket/list
                  append* remove-duplicates))
(require (only-in racket/set
                  set-subtract))

(provide (all-defined-out))

;; Types

(define-datatype type type?
  (int-type)
  (bool-type)
  (list-type
    (elem type?))
  (arrow-type
    (left type?)
    (right type?))
  (var-type
    (id integer?))
  (ref-type
    (elem type?))
  (tuple-type
    (types (list-of type?)))
  )

(define (ref-type? typ)
  (cases type typ
    (ref-type (elem) #t)
    (else #f)))

(define (arrow-type? typ)
  (cases type typ
    (arrow-type (left right) #t)
    (else #f)))

(define (tuple-type? typ)
  (cases type typ
    (tuple-type (types) #t)
    (else #f)))

(define (var-ids-in-type typ)
  (remove-duplicates
    (cases type typ
      (arrow-type (left right)
        (append (var-ids-in-type left) (var-ids-in-type right)))
      (list-type (elem)
        (var-ids-in-type elem))
      (ref-type (elem)
        (var-ids-in-type elem))
      (tuple-type (elems)
        (append* (map var-ids-in-type elems)))
      (int-type () '())
      (bool-type () '())
      (var-type (id) (list id)))))

(define (free-var-ids-in-tscheme tscheme)
  (cases type-scheme tscheme
    (a-type-scheme (quantified-ids quantified-type)
      (set-subtract
        (var-ids-in-type quantified-type)
        quantified-ids))))

;; Mutable interface for fresh type variables (var-type)

(define typevar-counter 'uninitialized)
(define (initialize-typevar-counter!)
  (set! typevar-counter 0))
(define (get-fresh-typevar-id)
  (set! typevar-counter (+ typevar-counter 1))
  typevar-counter)
(define (get-fresh-typevar)
  (var-type (get-fresh-typevar-id)))

;; Type schemes

(define-datatype type-scheme type-scheme?
  (a-type-scheme
    (quantified-ids (list-of integer?))
    (quantified-type type?)))

(define (a-type-scheme/simple typ)
  (a-type-scheme '() typ))
