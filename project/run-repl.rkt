#lang eopl

(require (only-in racket/base
                  read-line displayln eof-object? printf format
                  exit))
(require (only-in racket/string
                  string-join))
(require "lang.rkt")
(require "data-structures.rkt")
(require "prettyprint-exp.rkt")
(require (only-in "interp.rkt"
                  value-of-program))
(require (only-in "inference.rkt"
                  infer/pgm))
(require (only-in "prettyprint-type.rkt"
                  prettyprint-type))

(define prompt "> ")

(define (handle-exit)
  (exit))

(define (prettyprint-expval val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
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

(define (handle-interpret str)
  (let* ((prg (scan&parse str))
         (result-type (infer/pgm prg))
         (result-value (value-of-program prg)))
    (printf
      "~a : ~a\n\n"
      (prettyprint-expval result-value)
      (prettyprint-type result-type))))

(define (repl-rec)
  (display prompt)
  (let ((command-read (read-line)))
    (cond
      ((eof-object? command-read) (handle-exit))
      ((equal? command-read "exit") (handle-exit))
      (else (handle-interpret command-read))))
  (repl-rec))

(define (repl)
  (repl-rec))

(repl)
