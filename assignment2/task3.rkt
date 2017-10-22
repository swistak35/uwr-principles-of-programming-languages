#lang eopl

(require rackunit)

; Task 3 (2.11)

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (empty-env)
  '())

(define (extend-env* vars vals saved-env)
  (cons (cons vars vals) saved-env))

(define (rib-null? rib)
  (and (null? (car rib)) (null? (cdr rib))))

(define (rib-var rib)
  (car (car rib)))

(define (rib-val rib)
  (car (cdr rib)))

(define (rib-rest rib)
  (cons (cdr (car rib)) (cdr (cdr rib))))

(define (apply-env-rib rib env search-var)
  (cond
    ((rib-null? rib) (apply-env env search-var))
    ((eq? (rib-var rib) search-var) (rib-val rib))
    (else (apply-env-rib (rib-rest rib) env search-var))))

(define (apply-env env search-var)
  (if (null? env)
    (report-no-binding-found search-var)
    (apply-env-rib (car env) (cdr env) search-var)))

(check-equal?
  (apply-env
    (extend-env* '(x y) '(42 11) (empty-env))
    'x)
  42)
(check-equal?
  (apply-env
    (extend-env* '(x y) '(42 11) (empty-env))
    'y)
  11)
(check-equal?
  (apply-env
    (extend-env* '(x) '(42)
      (extend-env* '(y) '(11) (empty-env)))
    'y)
  11)
(check-equal?
  (apply-env
    (extend-env* '(x) '(42)
      (extend-env* '(y) '(11) (empty-env)))
    'x)
  42)
(check-exn
  #rx"No binding for x"
  (lambda ()
    (apply-env (empty-env) 'x)))
