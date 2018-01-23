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
  (tuple-type
    (types (list-of type?)))
  )

(define typevar-counter 'uninitialized)
(define (initialize-typevar-counter!)
  (set! typevar-counter 0))
(define (get-fresh-typevar-id)
  (set! typevar-counter (+ typevar-counter 1))
  typevar-counter)
(define (get-fresh-typevar)
  (var-type (get-fresh-typevar-id)))
