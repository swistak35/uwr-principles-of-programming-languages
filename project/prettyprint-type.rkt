#lang eopl

(require (only-in racket/base
                  format))
(require (only-in racket/string
                  string-join))
(require "type-data-structures.rkt")

(provide prettyprint-type prettyprint-tscheme)

(define (infix-type? typ)
  (cases type typ
    (arrow-type (l r) #t)
    (tuple-type (ts) #t)
    (else #f)))

(define (wrap s)
  (format "(~a)" s))

(define (prettyprint-tscheme tscheme)
  (cases type-scheme tscheme
    (a-type-scheme (quantified-ids quantified-type)
      (if (null? quantified-ids)
        (prettyprint-type quantified-type)
        (format
          "forall ~a . ~a"
          quantified-ids
          (prettyprint-type quantified-type))))))

(define (prettyprint-type typ)
  (cases type typ
    (int-type ()
      "int")
    (bool-type ()
      "bool")
    (list-type (elem-type)
      (format "~a list" (prettyprint-type elem-type)))
    (ref-type (elem-type)
      (format "~a ref" (prettyprint-type elem-type)))
    (arrow-type (left-type right-type)
      (let ((left-str (if (infix-type? left-type)
                        (wrap (prettyprint-type left-type))
                        (prettyprint-type left-type)))
            (right-str (prettyprint-type right-type)))
        (format "~a -> ~a" left-str right-str)))
    (var-type (id)
      (format "'~a" id))
    (tuple-type (types)
      (string-join
        (map
          (lambda (typ2) (if (infix-type? typ2)
                           (wrap (prettyprint-type typ2))
                           (prettyprint-type typ2)))
          types)
        " * "))
    ))
