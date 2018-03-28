#lang eopl

;; parser for the ARITH language

;; Program := Expression
;; Expression ::= Number | + (Expression, Expression)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program
     (expression)
     a-program)
    
    (expression
     (number)
     const-exp)
    (expression
     ("+" "(" expression "," expression ")")
     sum-exp)))
;;    (expression
;;     (expression "+" expression)
;;     sum-exp)))


;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; evaluation function ;;;;;;;;;;;;;;;

(define (eval-program p)
  (cases program p
    (a-program (e) (eval-expression e))))

(define (eval-expression e)
  (cases expression e
    (const-exp (n) n)
    (sum-exp (e1 e2)
             (+ (eval-expression e1)
                (eval-expression e2)))))

;;;;;;;;;;;;;;;;; REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-eval-print
  (sllgen:make-rep-loop
   "> "
   eval-program
   (sllgen:make-stream-parser the-lexical-spec the-grammar)))
