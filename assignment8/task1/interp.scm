(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  (define (try-continuation? cont)
    (or (continuation? cont) (eq? cont 'empty)))

  (define-datatype continuation continuation?
    (end-cont)
    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (try-cont try-continuation?)
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (try-cont try-continuation?)
      (val1 expval?)
      (cont continuation?))
    (unop-arg-cont
      (try-cont try-continuation?)
      (unop unary-op?)
      (cont continuation?))
    (if-test-cont
      (try-cont try-continuation?)
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (try-cont try-continuation?)
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (try-cont try-continuation?)
      (val1 expval?)
      (cont continuation?))
    (try-cont
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?))
    (raise1-cont
      (try-cont try-continuation?)
      (saved-cont continuation?))
    )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
          (value-of/k body (init-env) (end-cont))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont)
      (let ((current-try (cont->try cont)))
        (cases expression exp

          (const-exp (num) (apply-cont cont (num-val num)))

          (const-list-exp (nums)
            (apply-cont cont
              (list-val (map num-val nums))))

          (var-exp (var) (apply-cont cont (apply-env env var)))

          (diff-exp (exp1 exp2)
            (value-of/k exp1 env
              (diff1-cont current-try exp2 env cont)))

          (unop-exp (unop exp1)
            (value-of/k exp1 env
              (unop-arg-cont current-try unop cont)))

          (if-exp (exp1 exp2 exp3)
            (value-of/k exp1 env
              (if-test-cont current-try exp2 exp3 env cont)))

          (proc-exp (var body)
            (apply-cont cont
              (proc-val
                (procedure var body env))))

          (call-exp (rator rand)
            (value-of/k rator env
              (rator-cont current-try rand env cont)))

          ;; make let a macro, because I'm too lazy to add the extra
          ;; continuation
          (let-exp (var exp1 body)
            (value-of/k
              (call-exp (proc-exp var body) exp1)
              env
              cont))

          (letrec-exp (p-name b-var p-body letrec-body)
            (value-of/k
              letrec-body
              (extend-env-rec p-name b-var p-body env)
              cont))

          (try-exp (exp1 var handler-exp)
            (value-of/k exp1 env
              (try-cont var handler-exp env cont)))

          (raise-exp (exp1)
            (value-of/k exp1 env
              (raise1-cont current-try cont)))
          ))))

  ;; apply-cont : continuation * expval -> final-expval

  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () val)
        (diff1-cont (saved-try exp2 saved-env saved-cont)
          (value-of/k exp2 saved-env (diff2-cont saved-try val saved-cont)))
        (diff2-cont (saved-try val1 saved-cont)
          (let ((n1 (expval->num val1))
                (n2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- n1 n2)))))
        (unop-arg-cont (saved-try unop cont)
          (apply-cont cont
            (apply-unop unop val)))
        (if-test-cont (saved-try exp2 exp3 env cont)
          (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))
        (rator-cont (saved-try rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont saved-try val saved-cont)))
        (rand-cont (saved-try val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure proc val saved-cont)))
        ;; the body of the try finished normally-- don't evaluate the handler
        (try-cont (var handler-exp saved-env saved-cont)
          (apply-cont saved-cont val))
        ;; val is the value of the argument to raise
        (raise1-cont (saved-try saved-cont)
          ;; we put the short argument first to make the trace more readable.
          (apply-handler val saved-try))
        )))
      
  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define apply-handler
    (lambda (val saved-try-cont)
      (if (eq? saved-try-cont 'empty)
        (eopl:error 'apply-handler "uncaught exception!")
        (cases continuation saved-try-cont
          (try-cont (var handler-exp saved-env saved-cont)
            (value-of/k handler-exp
              (extend-env var val saved-env)
              saved-cont))

          (else (eopl:error 'apply-handler "impossible"))
          ))))

  ;; apply-handler : ExpVal * Cont -> FinalAnswer
  (define cont->try
    (lambda (cont)
      (cases continuation cont
             ;; interesting cases
             (try-cont (var handler-exp saved-env saved-cont) cont)
             (end-cont () 'empty)

             ;; otherwise, just look for the handler...
             (diff1-cont (saved-try exp2 saved-env saved-cont) saved-try)
             (diff2-cont (saved-try val1 saved-cont) saved-try)
             (if-test-cont (saved-try exp2 exp3 env saved-cont) saved-try)
             (unop-arg-cont (saved-try unop saved-cont) saved-try)
             (rator-cont (saved-try rand saved-env saved-cont) saved-try)
             (rand-cont (saved-try val1 saved-cont) saved-try)
             (raise1-cont (saved-try saved-cont) saved-try)
             )))


  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


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
