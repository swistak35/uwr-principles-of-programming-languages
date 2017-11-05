
#lang eopl

(require rackunit)
(require racket/trace)

(require (only-in racket/base
                  filter))

;;;;;;;;; environments

(define empty-env-record
  (lambda () 
    '()))

(define empty-env-record? null?)

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
              (symbol? (car (car x)))
              (expval? (cadr (car x)))
              (environment? (cdr x))))))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
      (eopl:error 'apply-env "No binding for ~s" search-sym)
      (let ((sym (extended-env-record->sym env))
            (val (extended-env-record->val env))
            (old-env (extended-env-record->old-env env)))
        (if (eqv? search-sym sym)
          val
          (apply-env old-env search-sym))))))

(define init-env empty-env)


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
      ("proc" "(" identifier ")" expression)
      proc-exp)

    (expression
      ("(" expression expression ")")
      call-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

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
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val 
    (proc proc?)))

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (env environment?)))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
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
            (num-val
              (- num1 num2)))))
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
      (proc-exp (var body)
        (proc-val (procedure var body (env-only-with-freevars env exp))))
      (call-exp (rator rand)
        (let ((proc (expval->proc (value-of rator env)))
              (arg (value-of rand env)))
          (apply-procedure proc arg)))
           )))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 79
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;;;;;;;; actual task

(define (freevars-without-var fvars var)
  (filter
    (lambda (v) (not (eq? v var)))
    fvars))

(define (get-freevars exp)
  (cases expression exp
         (const-exp (num) '())
         (var-exp (var) (list var))
         (diff-exp (exp1 exp2)
                   (let ((fvars-exp1 (get-freevars exp1))
                         (fvars-exp2 (get-freevars exp2)))
                     (append fvars-exp1 fvars-exp2)))
         (zero?-exp (exp1) (get-freevars exp1))
         (if-exp (exp1 exp2 exp3)
                 (append
                   (get-freevars exp1)
                   (get-freevars exp2)
                   (get-freevars exp3)))
         (let-exp (var exp1 body)
                  (append
                    (get-freevars exp1)
                    (freevars-without-var (get-freevars body) var)))
         (proc-exp (var body)
                   (freevars-without-var (get-freevars body) var))
         (call-exp (rator rand)
                   (append
                     (get-freevars rator)
                     (get-freevars rand)))
         ))

(define (restrict-env-to-vars env freevars)
  (filter
    (lambda (r) (member (car r) freevars))
    env))

(define (env-only-with-freevars env exp)
  (restrict-env-to-vars env (get-freevars exp)))

(check-equal? ; basic filtering of vars
  (env-only-with-freevars
    (extend-env 'x 42 (empty-env))
    (proc-exp 'x (const-exp 42)))
  (empty-env))
(check-equal? ; removes all occurrences of vars in env, not only one
  (env-only-with-freevars
    (extend-env 'x 42 (extend-env 'x 42 (empty-env)))
    (proc-exp 'x (const-exp 42)))
  (empty-env))
(check-equal? ; search for freevars deep in subexpressions
  (env-only-with-freevars
    (extend-env 'x 42 (extend-env 'y 42 (extend-env 'z 42 (empty-env))))
    (proc-exp
      'w
      (if-exp
        (zero?-exp (var-exp 'x))
        (var-exp 'y)
        (var-exp 'z))))
  (extend-env 'x 42 (extend-env 'y 42 (extend-env 'z 42 (empty-env)))))
(check-equal? ; search for freevars in let body
  (env-only-with-freevars
    (extend-env 'y 42 (empty-env))
    (proc-exp
      'x
      (let-exp
        'z
        (const-exp 42)
        (var-exp 'y))))
  (extend-env 'y 42 (empty-env)))
(check-equal? ; let also binds vars
  (env-only-with-freevars
    (extend-env 'y 42 (empty-env))
    (proc-exp
      'x
      (let-exp
        'y
        (const-exp 42)
        (var-exp 'y))))
  (empty-env))
(check-equal? ; removes vars which are not used at all
  (env-only-with-freevars
    (extend-env 'z 42 (empty-env))
    (proc-exp 'x (const-exp 42)))
  (empty-env))
