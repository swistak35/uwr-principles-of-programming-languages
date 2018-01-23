#lang eopl

(require "data-structures.rkt")
(provide empty-env extend-env apply-env)

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
      (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (bvar bval saved-env)
      (if (eqv? search-sym bvar)
        bval
        (apply-env saved-env search-sym)))
    (extend-env-rec* (p-names b-vars p-bodies saved-env)
      (cond 
        ((location search-sym p-names)
          => (lambda (n)
              (proc-val
                (procedure 
                  (list-ref b-vars n)
                  (list-ref p-bodies n)
                  env))))
        (else (apply-env saved-env search-sym))))))

(define (location sym syms)
  (cond
    ((null? syms) #f)
    ((eqv? sym (car syms)) 0)
    ((location sym (cdr syms))
      => (lambda (n) 
          (+ n 1)))
    (else #f)))
