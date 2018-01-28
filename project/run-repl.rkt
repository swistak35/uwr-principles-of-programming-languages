#lang eopl

(require (only-in racket/base
                  read-line displayln eof-object? printf
                  exit))

(define prompt "> ")

(define (handle-exit)
  (exit))

(define (handle-unknown cmd)
  (printf "Unknown input: ~s\n" cmd))

(define (repl-rec)
  (display prompt)
  (let ((command-read (read-line)))
    (cond
      ((eof-object? command-read) (handle-exit))
      ((equal? command-read "exit") (handle-exit))
      (else (handle-unknown command-read))))
  (repl-rec))

(define (repl)
  (repl-rec))

(repl)
