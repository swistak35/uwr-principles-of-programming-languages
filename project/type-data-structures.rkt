#lang eopl

(provide (all-defined-out))

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

(define-datatype type-scheme type-scheme?
  (a-type-scheme
    (quantified-ids (list-of integer?))
    (quantified-type type?)))

(define typevar-counter 'uninitialized)
(define (initialize-typevar-counter!)
  (set! typevar-counter 0))
(define (get-fresh-typevar-id)
  (set! typevar-counter (+ typevar-counter 1))
  typevar-counter)
(define (get-fresh-typevar)
  (var-type (get-fresh-typevar-id)))
