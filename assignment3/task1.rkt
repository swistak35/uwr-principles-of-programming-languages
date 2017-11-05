#lang eopl

; Tasks 1, 2 and 3 (extensions on let language)

(require rackunit)

(require (only-in racket/base
           foldl))

;;;;;;;;; environment

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

;;;;;;;;; grammatical spec

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression
      ("-" "(" expression "," expression ")")
      diff-exp)
    (expression
      ("zero?" "(" expression ")")
      zero?-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression (identifier) var-exp)
    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp)   
    (expression
      ("let*" (arbno identifier "=" expression) "in" expression)
      let*-exp)   
    (expression
      ("emptylist")
      emptylist-exp)
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp)
    (expression
      ("car" "(" expression ")")
      car-exp)
    (expression
      ("cdr" "(" expression ")")
      cdr-exp)
    (expression
      ("null?" "(" expression ")")
      null?-exp)
    (expression
      ("list" "(" (separated-list expression ",") ")")
      list-exp)
    ))

;;;;;;;;; sllgen boilerplate

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;; interpreter

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (list-val
    (lst list?)))

(define report-expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

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

      ; let*
      (let*-exp (vars exps body)
        (let ((new-env (foldl
                          (lambda (var1 exp1 env1)
                            (extend-env var1 (value-of exp1 env1) env1))
                          env vars exps)))
          (value-of body new-env)))

      ; list operators
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
          (bool-val (null? lst1))))
      (list-exp (exps)
        (let ((exps-vals (map (lambda (e) (value-of e env)) exps)))
          (list-val (apply list exps-vals))))
      )))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;;;;;;;; interpreter tests

; ExpVal = Int + Bool + List
; DenVal = Int + Bool + List

(check-equal? ; interpreter test without new extensions
  (run "-(30,2)")
  (num-val 28))
(check-equal? ; empty list
  (run "emptylist")
  (list-val '()))
(check-equal? ; consed list using variables
  (run "let x = 42 in cons(x, cons(x, emptylist))")
  (list-val (list (num-val 42) (num-val 42))))
(check-equal? ; nested lists construction
  (run "cons(cons(42, emptylist), cons(cons(24, emptylist), emptylist))")
  (list-val (list (list-val (list (num-val 42))) (list-val (list (num-val 24))))))
(check-equal? ; car() test
  (run "let x = 42 in car(cons(x, cons(x, emptylist)))")
  (num-val 42))
(check-equal? ; cdr() test
  (run "let x = 42 in cdr(cons(x, cons(x, emptylist)))")
  (list-val (list (num-val 42))))
(check-equal? ; null?() test
  (run "null?(cons(42, emptylist))")
  (bool-val #f))
(check-equal? ; null?() test
  (run "let x = emptylist in null?(x)")
  (bool-val #t))
(check-equal? ; list() test
  (run "let x = 42 in list(41, x, 43)")
  (list-val (list (num-val 41) (num-val 42) (num-val 43))))
(check-equal? ; list() empty test
  (run "list()")
  (list-val (list)))
(check-equal? ; let* works like usual let
  (run "let* x = 42 in -(x, 2)")
  (num-val 40))
(check-equal? ; let* may assign multiple variables
  (run "let* x = 42 y = 22 in -(x, y)")
  (num-val 20))
(check-equal? ; let* is binding immediately
  (run "let x = 30 in let* x = -(x, 1) y = -(x, 2) in -(x, y)")
  (num-val 2))
