#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")
(require (only-in racket/base
                  format))
(require (only-in racket/string
                  string-join))

(provide prettyprint-exp)

(define (prettyprint-exp exp)
  (cases expression exp

    (const-exp (num) (number->string num))

    (var-exp (var)
      (symbol->string var))

    (if-exp (exp1 exp2 exp3)
      (format
        "if ~a then ~a else ~a"
        (prettyprint-exp exp1)
        (prettyprint-exp exp2)
        (prettyprint-exp exp3)))

    (let-exp (var exp1 body)
      (format
        "let ~a = ~a in ~a"
        (symbol->string var)
        (prettyprint-exp exp1)
        (prettyprint-exp body)))

    (proc-exp (var body)
      (format
        "proc(~a) ~a"
        (symbol->string var)
        (prettyprint-exp body)))

    (call-exp (rator rands)
      (format
        "(~a ~a)"
        (prettyprint-exp rator)
        (string-join (map prettyprint-exp rands) " ")))

    (letrec-exp (p-names list-of-b-vars p-bodies letrec-body)
      (format
        "letrec ~a in ~a"
        (string-join
          (map
            (lambda (p-name b-vars p-body)
              (format
                "~a(~a) = ~a"
                p-name
                (string-join (map symbol->string b-vars) ", ")
                (prettyprint-exp p-body)))
            p-names list-of-b-vars p-bodies)
          "  ")
        (prettyprint-exp letrec-body)))

    (begin-exp (exp1 exps)
      (format
        "begin ~a end"
        (string-join (map prettyprint-exp (cons exp1 exps)) "; ")))

    (list-exp (exps)
      (format
        "[~a]"
        (string-join
          (map prettyprint-exp exps)
          ", ")
        ))

    ))
