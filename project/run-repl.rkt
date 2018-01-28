#lang eopl

(require (only-in racket/base
                  read-line displayln eof-object? printf
                  exit with-handlers exn-message))
(require racket/exn)
(require "lang.rkt")
(require (only-in "interp.rkt"
                  value-of-program))
(require (only-in "inference.rkt"
                  infer-program))
(require (only-in "prettyprint-type.rkt"
                  prettyprint-type))
(require (only-in "prettyprint-expval.rkt"
                  prettyprint-expval))

(define prompt "> ")

(define (handle-exit)
  (exit))

(define (handle-interpret str)
  (let* ((prg (scan&parse str))
         (result-type (infer-program prg))
         (result-value (value-of-program prg)))
    (printf
      "~a : ~a\n\n"
      (prettyprint-expval result-value)
      (prettyprint-type result-type))))

(define (repl-rec)
  (with-handlers ([(lambda (exn) #t) (lambda (e) (display (exn-message e)) (repl-rec))])
    (display prompt)
    (let ((command-read (read-line)))
      (cond
        ((eof-object? command-read) (handle-exit))
        ((equal? command-read "exit") (handle-exit))
        (else (handle-interpret command-read)))))
  (repl-rec))

(define (repl)
  (repl-rec))

(repl)
