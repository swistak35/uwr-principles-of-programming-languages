#lang eopl

(require rackunit)
(require trace)

; Task 8 (B.1)

(define scanner-spec-1
  '((white-sp (whitespace) skip)
    (intgr (digit (arbno digit)) number)
    (operator ((or "+" "-" "*" "/")) string)))

(define grammar-1
  '((arith-expr
      (arith-term (arbno additive-op arith-term))
      a-arith-expr)
    (arith-term
      (arith-factor (arbno multiplicative-op arith-factor))
      a-arith-term)
    (arith-factor
      (intgr)
      arith-factor-number)
    (arith-factor
      ("(" arith-expr ")")
      arith-factor-expr)
    (additive-op
      ("+")
      additive-op-plus)
    (additive-op
      ("-")
      additive-op-minus)
    (multiplicative-op
      ("*")
      multiplicative-op-mult)
    (multiplicative-op
      ("/")
      multiplicative-op-div)))

(sllgen:make-define-datatypes scanner-spec-1 grammar-1)

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-1 grammar-1)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-1 grammar-1))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-1 grammar-1))

(check-equal?
  (scan&parse "1 + 2")
  (a-arith-expr
    (a-arith-term (arith-factor-number 1) '() '())
    (list (additive-op-plus))
    (list (a-arith-term (arith-factor-number 2) '() '()))))

(check-equal?
  (scan&parse "3 + 2 * 66 - 5")
  (a-arith-expr
    (a-arith-term (arith-factor-number 3) '() '())
    (list
      (additive-op-plus)
      (additive-op-minus))
    (list
      (a-arith-term
        (arith-factor-number 2)
        (list (multiplicative-op-mult))
        (list (arith-factor-number 66)))
      (a-arith-term (arith-factor-number 5) '() '()))))

; Task 9 (B.3)

(define (additive-op-fun op)
  (cases additive-op op
    (additive-op-plus () +)
    (additive-op-minus () -)))

(define (value-of-aexp aexp)
  (cases arith-expr aexp
    (a-arith-expr (aterm-1 a-ops a-terms)
      (if (null? a-ops)
        (value-of-aterm aterm-1)
          (value-of-aexp
            (a-arith-expr
              (a-arith-term
                (arith-factor-number
                  ((additive-op-fun (car a-ops))
                    (value-of-aterm aterm-1)
                    (value-of-aterm (car a-terms)))) '() '())
              (cdr a-ops)
              (cdr a-terms)))))))

(define (multiplicative-op-fun op)
  (cases multiplicative-op op
    (multiplicative-op-mult () *)
    (multiplicative-op-div () quotient)))

(define (value-of-aterm aterm)
  (cases arith-term aterm
    (a-arith-term (afactor-1 a-ops a-factors)
      (if (null? a-ops)
        (value-of-afactor afactor-1)
        (value-of-aterm
          (a-arith-term
            (arith-factor-number
              ((multiplicative-op-fun (car a-ops))
                (value-of-afactor afactor-1)
                (value-of-afactor (car a-factors))))
            (cdr a-ops)
            (cdr a-factors)))))))

(define (value-of-afactor afactor)
  (cases arith-factor afactor
    (arith-factor-number (n) n)
    (arith-factor-expr (aexp) (value-of-aexp aexp))))

(define (value-of str)
  (value-of-aexp (scan&parse str)))

; (trace value-of-aexp)
; (trace value-of-aterm)
; (trace value-of-afactor)

; tests for basic ops
(check-equal? (value-of "3 + 5") 8)
(check-equal? (value-of "5 - 3") 2)
(check-equal? (value-of "4 * 3") 12)
(check-equal? (value-of "4 / 3") 1)
; tests for associativity
(check-equal? (value-of "8 - 5 - 2") 1)
(check-equal? (value-of "18 / 5 / 2") 1)
; more complex tests (i.e. parenthesis)
(check-equal? (value-of "4 + 3 - 5") 2)
(check-equal? (value-of "4 + 3 * 2 - 5") 5)
(check-equal? (value-of "(4 + 3) * 2 - 5") 9)
(check-equal? (value-of "2 * (3 + (4 + 3) / 2) - 2") 10)


; REPL
(define read-eval-print
  (sllgen:make-rep-loop "--> " (lambda (aexp) (value-of-aexp aexp))
    (sllgen:make-stream-parser scanner-spec-1 grammar-1)))

(read-eval-print)
