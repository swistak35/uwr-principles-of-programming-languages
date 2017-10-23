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

; (define read-eval-print
;   (sllgen:make-rep-loop "--> " value-of--program
;                         (sllgen:make-stream-parser scanner-spec-1 grammar-1)))

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

; (eopl:pretty-print (scan&parse "3 + 2 * 66 - 5"))
