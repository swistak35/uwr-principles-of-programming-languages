#lang eopl

(require (only-in racket/string
                  string-join))
(require (only-in racket/base
                  format))
(require "data-structures.rkt")
(require (only-in "prettyprint-exp.rkt"
                  prettyprint-exp))

(provide prettyprint-expval)

(define (prettyprint-expval val)
  (cases expval val
    (num-val (num) (number->string num))
    (bool-val (bool) (if bool "#t" "#f"))
    (list-val (lst)
    (format
      "[~a]"
      (string-join (map prettyprint-expval lst) ", ")))
    (ref-val (ref)
    (format "&~a" ref))
    (proc-val (p)
    (cases proc p
      (procedure (bvars body env)
        (format
          "procedure<(~a); ~a>"
          (string-join (map symbol->string bvars) ", ")
          (prettyprint-exp body)))
      (primitive (name)
        (format "primitive<~a>" name))))))


