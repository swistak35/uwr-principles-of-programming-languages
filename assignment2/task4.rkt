#lang eopl

(require rackunit)

; Task 4 (2.21)

(define (anything? x)
  #t)

(define-datatype env env?
  (extend-env
    (var symbol?)
    (val anything?)
    (saved-env env?))
  (empty-env))

(define (has-binding? search-env search-var)
  (cases env search-env
    (extend-env (var val saved-env)
      (if (eq? var search-var)
        #t
        (has-binding? saved-env search-var)))
    (empty-env ()
      #f)))

(check-true
  (has-binding?
    (extend-env 'x 42 (empty-env))
    'x))

(check-false
  (has-binding?
    (extend-env 'x 42 (empty-env))
    'y))

(check-false
  (has-binding?
    (empty-env)
    'y))
