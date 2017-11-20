(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LEXADDR language.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-translation value-of)

  (require (only-in racket/base
                    filter exact-nonnegative-integer? sort))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-translation : Nameless-program -> ExpVal

  (define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))

  ;; value-of-translation : Nameless-program -> ExpVal
  ;; Page: 100
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))
  
  ;; value-of : Nameless-exp * Nameless-env -> ExpVal
  (define value-of
    (lambda (exp nameless-env)
      (cases expression exp
                (const-exp (num) (num-val num))

        (diff-exp (exp1 exp2)
          (let ((val1
                  (expval->num
                    (value-of exp1 nameless-env)))
                (val2
                  (expval->num
                    (value-of exp2 nameless-env))))
            (num-val
              (- val1 val2))))
        
        (zero?-exp (exp1)
          (let ((val1 (expval->num (value-of exp1 nameless-env))))
            (if (zero? val1)
              (bool-val #t)
              (bool-val #f))))

        (if-exp (exp0 exp1 exp2) 
          (if (expval->bool (value-of exp0 nameless-env))
            (value-of exp1 nameless-env)
            (value-of exp2 nameless-env)))

        (call-exp (rator rand)          
          (let ((proc (expval->proc (value-of rator nameless-env)))
                (arg (value-of rand nameless-env)))
	    (apply-procedure proc arg)))

        (nameless-var-exp (n)
          (apply-nameless-env nameless-env n))

        (nameless-let-exp (exp1 body)
          (let ((val (value-of exp1 nameless-env)))
            (value-of body
              (extend-nameless-env val nameless-env))))

        (nameless-proc-exp (body)
          (proc-val
              (procedure body
                (nenv-only-with-freevars nameless-env body))))

        (else
         (eopl:error 'value-of 
	    "Illegal expression in translated code: ~s" exp))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (body saved-env)
          (value-of body (extend-nameless-env arg saved-env))))))

  ;; filtering of freevars for nameless environments goes below

  (define (sub1 x) (- x 1))

  (define (freevars-without-zero fvars)
    (filter
      exact-nonnegative-integer?
      (map sub1 fvars)))

  (define (get-nenv-freevars exp)
    (cases expression exp
           (const-exp (num) '())
           (diff-exp (exp1 exp2)
                     (let ((fvars-exp1 (get-nenv-freevars exp1))
                           (fvars-exp2 (get-nenv-freevars exp2)))
                       (append fvars-exp1 fvars-exp2)))
           (zero?-exp (exp1) (get-nenv-freevars exp1))
           (if-exp (exp1 exp2 exp3)
                   (append
                     (get-nenv-freevars exp1)
                     (get-nenv-freevars exp2)
                     (get-nenv-freevars exp3)))
           (call-exp (rator rand)
                     (append
                       (get-nenv-freevars rator)
                       (get-nenv-freevars rand)))
           (nameless-var-exp (var) (list var))
           (nameless-let-exp (exp1 body)
                             (append
                               (get-nenv-freevars exp1)
                               (freevars-without-zero (get-nenv-freevars body))))
           (nameless-proc-exp (body)
                              (freevars-without-zero (get-nenv-freevars body)))
           (else (eopl:error 'value-of 
                             "Illegal expression in translated code: ~s" exp))
           ))

  (define (restrict-nenv-to-vars nenv freevars)
    (map
      (lambda (i) (list-ref nenv i))
      (sort freevars <)))

  (define (nenv-only-with-freevars nenv exp)
    (restrict-nenv-to-vars nenv (get-nenv-freevars exp)))


  )
