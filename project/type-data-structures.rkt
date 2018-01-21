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
    (id integer?)))
