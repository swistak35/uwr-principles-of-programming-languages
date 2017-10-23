#lang eopl

(require rackunit)

; Task 5 (2.28)

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

; (define format
;   (lambda args
;     (let ((o (open-output-string)))
;       (apply fprintf (cons o args))
;       (get-output-string o))))

(define (unparse exp)
  (cases lc-exp exp
    (var-exp (var) (symbol->string var))
    (lambda-exp (bound-var body) (string-append "(lambda (" (symbol->string bound-var) ") " (unparse body) ")"))
    (app-exp (rator rand) (string-append "(" (unparse rator) " " (unparse rand) ")"))))

(check-equal?
  (unparse
    (var-exp 'x))
  "x")

(check-equal?
  (unparse
    (app-exp (var-exp 'x) (var-exp 'y)))
  "(x y)")

(check-equal?
  (unparse
    (lambda-exp 'x (var-exp 'y)))
  "(lambda (x) y)")

(check-equal?
  (unparse
    (lambda-exp 'x
      (app-exp
        (var-exp 'y) (var-exp 'z))))
  "(lambda (x) (y z))")
