#lang eopl

(require rackunit)
(require trace)

; Task 6 (2.29)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-vars (list-of identifier?))
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'parse-expression "Invalid syntax ~s" datum))

(define (parse-expression datum)
  (cond
    ((symbol? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
       (lambda-exp
         (cadr datum)
         (parse-expression (caddr datum)))
       (app-exp
         (parse-expression (car datum))
         (map parse-expression (cdr datum)))))
    (else (report-invalid-concrete-syntax datum))))

(display (parse-expression '(lambda (x) (+ x y))))

(check-equal?
  (parse-expression '(lambda (x) (+ x y)))
  (lambda-exp
    '(x)
    (app-exp
      (var-exp '+)
      (list (var-exp 'x) (var-exp 'y)))))

(check-equal?
  (parse-expression '(lambda (x y) z))
  (lambda-exp '(x y) (var-exp 'z)))

(check-exn
  #rx"Invalid syntax 42"
  (lambda ()
    (parse-expression '(42 byzantine-keyword))))
