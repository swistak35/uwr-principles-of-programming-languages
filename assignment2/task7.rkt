#lang eopl

(require rackunit)
(require trace)

; Task 7 (2.31)

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'parse-expression "Invalid syntax ~s" datum))

(define (parse-expression list)
  (let ((res (parse-exp list)))
    (if (null? (cdr res))
      (car res)
      (report-invalid-concrete-syntax list))))

(define (parse-exp list)
  (cond
    ((eq? (car list) '-)
     (let* ((res-1 (parse-exp (cdr list)))
            (exp-1 (car res-1))
            (tail-1 (cdr res-1))
            (res-2 (parse-exp tail-1))
            (exp-2 (car res-2))
            (tail-2 (cdr res-2)))
      (cons
        (diff-exp exp-1 exp-2)
        tail-2)))
    ((integer? (car list))
     (cons
       (const-exp (car list))
       (cdr list)))
    (else (report-invalid-concrete-syntax list))))

; (trace parse-expression)
; (trace diff-exp)
; (trace const-exp)

(display (parse-expression '(- 4 3)))

(check-equal?
  (parse-expression '(- 4 3))
  (diff-exp
    (const-exp 4)
    (const-exp 3)))

(check-equal?
  (parse-expression '(- - 3 2 - 4 - 12 7))
  (diff-exp
    (diff-exp
      (const-exp 3)
      (const-exp 2))
    (diff-exp
      (const-exp 4)
      (diff-exp
        (const-exp 12)
        (const-exp 7)))))

(check-exn
  #rx"Invalid syntax"
  (lambda ()
    (parse-expression '(- 3 3 3))))
