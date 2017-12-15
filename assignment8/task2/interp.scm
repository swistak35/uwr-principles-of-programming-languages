(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


  (define-datatype continuation continuation?
    (end-cont)                          ; []
    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (val1 expval?)
      (cont continuation?))
    (unop-arg-cont
      (unop unary-op?)
      (cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (val1 expval?)
      (cont continuation?))
    (try-cont
      (cont continuation?)
      (saved-catch catch-continuation?))
    (raise1-cont
      (saved-cont continuation?))
    )

  (define-datatype catch-continuation catch-continuation?
    (no-catch)
    (a-catch
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?)
      (saved-catch catch-continuation?))
    )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (end-cont) (no-catch))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont catch-cont)
      (cases expression exp

        (const-exp (num) (apply-cont cont catch-cont (num-val num)))

        (const-list-exp (nums)
          (apply-cont cont catch-cont
            (list-val (map num-val nums))))

        (var-exp (var) (apply-cont cont catch-cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)
            catch-cont))

        (unop-exp (unop exp1)
          (value-of/k exp1 env
            (unop-arg-cont unop cont)
            catch-cont))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)
            catch-cont))

        (proc-exp (var body)
          (apply-cont cont catch-cont
            (proc-val
              (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont)
            catch-cont))

        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
          (value-of/k
            (call-exp (proc-exp var body) exp1)
            env
            cont
            catch-cont))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont
            catch-cont))

        (try-exp (exp1 var handler-exp)
          (value-of/k exp1 env
            (try-cont cont catch-cont)
            (a-catch var handler-exp env cont catch-cont)))

        (raise-exp (exp1)
          (value-of/k exp1 env
            (raise1-cont cont)
            catch-cont))
        )))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont catch-cont val)
      (cases continuation cont
        (end-cont () val)
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2 saved-env (diff2-cont val saved-cont) catch-cont))
        (diff2-cont (val1 saved-cont)
          (let ((n1 (expval->num val1))
                (n2 (expval->num val)))
            (apply-cont saved-cont catch-cont
              (num-val (- n1 n2)))))
        (unop-arg-cont (unop cont)
          (apply-cont cont catch-cont
            (apply-unop unop val)))
        (if-test-cont (exp2 exp3 env cont)
          (if (expval->bool val)
            (value-of/k exp2 env cont catch-cont)
            (value-of/k exp3 env cont catch-cont)))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont) catch-cont))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure proc val saved-cont catch-cont)))
        ;; the body of the try finished normally-- don't evaluate the handler
        (try-cont (saved-cont saved-catch)
          (apply-cont saved-cont saved-catch val))
        ;; val is the value of the argument to raise
        (raise1-cont (saved-cont)
          ;; we put the short argument first to make the trace more readable.
          (apply-handler val catch-cont))
        )))
      
  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (val catch-cont)
      (cases catch-continuation catch-cont
        (no-catch ()
                  (eopl:error 'apply-handler "uncaught exception!"))
        (a-catch (var handler-exp saved-env saved-cont saved-catch)
          (value-of/k handler-exp
            (extend-env var val saved-env)
            saved-cont
            saved-catch))
        )))


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont catch-cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont
            catch-cont)))))


  (define apply-unop
    (lambda (unop val)
      (cases unary-op unop
        (null?-unop ()
          (bool-val
            (null? (expval->list val))))
        (car-unop ()
          (car (expval->list val)))
        (cdr-unop ()
          (list-val (cdr (expval->list val))))
        (zero?-unop ()
          (bool-val
            (zero? (expval->num val)))))))


  ;; to get the detailed trace:
  ;; (trace value-of/k apply-cont apply-handler)

  )
