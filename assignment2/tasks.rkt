#lang eopl

(require rackunit)

; Task 1 (2.5)

(define (empty-env)
  '())

(define (extend-env var val saved-env)
  (cons (cons var val) saved-env))

(define (env-var env)
  (car (car env)))
(define (env-val env)
  (cdr (car env)))
(define (env-rest env)
  (cdr env))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (apply-env env search-var)
  (cond
    ((null? env) (report-no-binding-found search-var))
    ((eq? search-var (env-var env)) (env-val env))
    (else (apply-env (env-rest env) search-var))))

(check-equal? (empty-env) '())
(check-equal? (extend-env 'x 42 (empty-env)) '((x . 42)))
(check-equal? (extend-env 'y 11 (extend-env 'x 42 (empty-env))) '((y . 11) (x . 42)))
(check-equal? (extend-env 'x 11 (extend-env 'x 42 (empty-env))) '((x . 11) (x . 42)))
(check-equal? (apply-env (extend-env 'x 42 (empty-env)) 'x) 42)
(check-equal? (apply-env (extend-env 'x 11 (extend-env 'x 42 (empty-env))) 'x) 11)
(check-exn #rx"No binding for x" (lambda () (apply-env (empty-env) 'x)))
