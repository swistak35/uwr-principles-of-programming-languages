#lang eopl

(require rackunit)

(define (init-env)
  '())
(define (extend-env var val saved-env)
  (cons (cons var val) saved-env))
(define (env-var env)
  (car (car env)))
(define (env-val env)
  (cdr (car env)))
(define (env-rest env)
  (cdr env))
(define (apply-env env search-var)
  (cond
    ((null? env) (report-no-binding-found search-var))
    ((eq? search-var (env-var env)) (env-val env))
    (else (apply-env (env-rest env) search-var))))
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define identifier? symbol?)

(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
    (num number?))
  (diff-exp
    (exp1 expression?)
    (exp2 expression?))
  (zero?-exp
    (exp1 expression?))
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?))
  (var-exp
    (var identifier?))
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?))
  (emptylist-exp)
  (cons-exp
    (exp-car expression?)
    (exp-cdr expression?))
  (car-exp
    (exp1 expression?))
  (cdr-exp
    (exp1 expression?))
  (null?-exp
    (exp1 expression?))
  (list-exp
    (exp1 (arbno exps))))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (list-val
    (lst list?)))

(define (report-expval-extractor-error num val)
  (eopl:error 'expval "expval-extractor-error ~s ~s" num val))

(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (report-expval-extractor-error 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
           (list-val (lst) lst)
           (else (report-expval-extractor-error 'lst val)))))

; (define run
;   (lambda (string)
;     (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (- num1 num2)))))
      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (if (zero? num1)
              (bool-val #t)
              (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env))))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body
            (extend-env var val1 env))))
      (emptylist-exp () (list-val '()))
      (cons-exp (car-exp cdr-exp)
        (let* ((val-car (value-of car-exp env))
               (val-cdr (value-of cdr-exp env))
               (val-cdr-list (expval->list val-cdr)))
          (list-val (cons val-car val-cdr-list))))
      (car-exp (exp1)
        (let* ((val-exp1 (value-of exp1 env))
               (lst1 (expval->list val-exp1)))
          (car lst1)))
      (cdr-exp (exp1)
        (let* ((val-exp1 (value-of exp1 env))
               (lst1 (expval->list val-exp1)))
          (list-val (cdr lst1))))
      (null?-exp (exp1)
        (let* ((val-exp1 (value-of exp1 env))
               (lst1 (expval->list val-exp1)))
          (bool-val (null? lst1)))))))

(check-equal?
  (value-of-program
    (a-program
      (emptylist-exp)))
  (list-val '()))
(check-equal?
  (value-of-program
    (a-program
      (let-exp
        'x
        (const-exp 42)
        (cons-exp
          (var-exp 'x)
          (cons-exp
            (var-exp 'x)
            (emptylist-exp))))))
  (list-val (list (num-val 42) (num-val 42))))
(check-equal?
  (value-of-program
    (a-program
      (cons-exp
        (cons-exp (const-exp 42) (emptylist-exp))
        (cons-exp
          (cons-exp (const-exp 24) (emptylist-exp))
          (emptylist-exp)))))
  (list-val (list (list-val (list (num-val 42))) (list-val (list (num-val 24))))))
(check-equal?
  (value-of-program
    (a-program
      (let-exp
        'x
        (const-exp 42)
        (car-exp
          (cons-exp
            (var-exp 'x)
            (cons-exp
              (var-exp 'x)
              (emptylist-exp)))))))
  (num-val 42))
(check-equal?
  (value-of-program
    (a-program
      (let-exp
        'x
        (const-exp 42)
        (cdr-exp
          (cons-exp
            (var-exp 'x)
            (cons-exp
              (var-exp 'x)
              (emptylist-exp)))))))
  (list-val (list (num-val 42))))
(check-equal?
  (value-of-program
    (a-program
      (null?-exp
        (cons-exp
          (const-exp 42)
          (emptylist-exp)))))
  (bool-val #f))
(check-equal?
  (value-of-program
    (a-program
      (let-exp
        'x
        (emptylist-exp)
        (null?-exp (var-exp 'x)))))
  (bool-val #t))

; TODO: Add parser rules
; TODO: Tests for parser rules
; TODO: list-exp should take ANY number of arguments, now it's 1+
