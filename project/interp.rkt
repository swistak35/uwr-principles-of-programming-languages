#lang eopl

(require (only-in racket/base
                  foldl))
(require "drscheme-init.scm")
(require "lang.scm")
(require "data-structures.scm")
(require "environments.scm")
(require "store.scm")

(provide value-of-program value-of instrument-newref)

;;; Initial environment when running a program

(define primitives-names
  '(diff zero? newref deref setref cons car cdr null?))

(define (initial-env)
  (foldl
    (lambda (p res) (extend-env p (proc-val (primitive p)) res))
    (empty-env)
    primitives-names))

;; Main interpreter

(define value-of-program 
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (initial-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp

      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env))))

      (let-exp (var exp1 body)       
        (let ((val1 (value-of exp1 env)))
          (value-of body
            (extend-env var val1 env))))
      
      (proc-exp (var body)
        (proc-val (procedure (list var) body env)))

      (call-exp (rator rands)
        (let ((proc (expval->proc (value-of rator env)))
              (args (map (lambda (rand) (value-of rand env)) rands)))
          (apply-procedure proc args)))

      (letrec-exp (p-names b-vars p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec* p-names b-vars p-bodies env)))

      (begin-exp (exp1 exps)
        (letrec 
          ((value-of-begins
              (lambda (e1 es)
                (let ((v1 (value-of e1 env)))
                  (if (null? es)
                    v1
                    (value-of-begins (car es) (cdr es)))))))
          (value-of-begins exp1 exps)))

      (list-exp (exps)
        (let ((exps-vals (map (lambda (e) (value-of e env)) exps)))
          (list-val exps-vals)))

      )))

;;; Calling procedures and primitives

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (vars body saved-env)
        (check-args (length vars) args 'procedure)
        (let ((new-env (foldl
                          (lambda (var arg res-env) (extend-env var arg res-env))
                          saved-env
                          vars args)))
          (value-of body new-env)))
      (primitive (name)
        (apply-primitive name args)))))

(define (apply-primitive name args)
  (cond
    ((eq? name 'diff) (primitive-diff args))
    ((eq? name 'zero?) (primitive-zero? args))
    ((eq? name 'newref) (primitive-newref args))
    ((eq? name 'deref) (primitive-deref args))
    ((eq? name 'setref) (primitive-setref args))
    ((eq? name 'cons) (primitive-cons args))
    ((eq? name 'car) (primitive-car args))
    ((eq? name 'cdr) (primitive-cdr args))
    ((eq? name 'null?) (primitive-null? args))
    (else (eopl:error 'apply-primitive "Unknown primitive ~s" name))))

(define (primitive-diff args)
  (check-args 2 args 'diff)
  (num-val
    (- (expval->num (car args)) (expval->num (cadr args)))))

(define (primitive-zero? args)
  (check-args 1 args 'zero?)
  (bool-val (zero? (expval->num (car args)))))

(define (primitive-newref args)
  (check-args 1 args 'newref)
  (ref-val (newref (car args))))

(define (primitive-deref args)
  (check-args 1 args 'deref)
  (deref (expval->ref (car args))))

(define (primitive-setref args)
  (check-args 2 args 'setref)
  (begin
    (setref! (expval->ref (car args)) (cadr args))
    (num-val 23)))

(define (primitive-cons args)
  (check-args 2 args 'cons)
  (list-val (cons (car args) (expval->list (cadr args)))))

(define (primitive-car args)
  (car (expval->list (car args))))

(define (primitive-cdr args)
  (list-val (cdr (expval->list (car args)))))

(define (primitive-null? args)
  (bool-val (null? (expval->list (car args)))))

;;; Small helper functions

(define (check-args expected-num args name)
  (let ((actual-num (length args)))
    (when (not (equal? expected-num actual-num))
      (eopl:error 'wrong-args "Wrong number of arguments to ~s, given ~s expected ~s" name actual-num expected-num))))
