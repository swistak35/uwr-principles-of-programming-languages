(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

  (require (only-in racket/base
                    foldl))
  (require trace)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  (define-datatype answer answer?
    (an-answer
      (val expval?)
      (store store?)))

  (define (answer->val a)
    (cases answer a
           (an-answer (val store) val)))
  (define (answer->store a)
    (cases answer a
           (an-answer (val store) store)))

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (answer->val (value-of exp1 (init-env) (empty-store)))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num)
                   (an-answer (num-val num) store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var)
                 (an-answer (apply-env env var) store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let* ((ans1 (value-of exp1 env store))
                 (ans2 (value-of exp2 env (answer->store ans1)))
                 (num1 (expval->num (answer->val ans1)))
                 (num2 (expval->num (answer->val ans2))))
              (an-answer (num-val (- num1 num2)) (answer->store ans2))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((ans1 (value-of exp1 env store)))
            (let ((num1 (expval->num (answer->val ans1))))
              (an-answer
                (if (zero? num1)
                  (bool-val #t)
                  (bool-val #f))
                (answer->store ans1)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((ans1 (value-of exp1 env store)))
            (if (expval->bool (answer->val ans1))
              (value-of exp2 env (answer->store ans1))
              (value-of exp3 env (answer->store ans1)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((ans1 (value-of exp1 env store)))
            (value-of body
              (extend-env var (answer->val ans1) env) (answer->store ans1))))
        
        (proc-exp (var body)
          (an-answer (proc-val (procedure var body env)) store))

        (call-exp (rator rand)
          (let* ((ans-rator (value-of rator env store))
                 (proc (expval->proc (answer->val ans-rator)))
                 (ans-rand (value-of rand env (answer->store ans-rator))))
            (apply-procedure proc (answer->val ans-rand) (answer->store ans-rand))))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env) store))

        (begin-exp (exp1 exps)
          (foldl
            (lambda (e result-ans) (value-of e env (answer->store result-ans)))
            (value-of exp1 env store)
            exps))
          ; (letrec 
          ;   ((value-of-begins
          ;      (lambda (e1 es)
          ;        (let ((v1 (value-of e1 env)))
          ;          (if (null? es)
          ;            v1
          ;            (value-of-begins (car es) (cdr es)))))))
          ;   (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let* ((ans1 (value-of exp1 env store))
                 (new-ref-store-pair (newref (answer->store ans1) (answer->val ans1)))
                 (new-ref (car new-ref-store-pair))
                 (new-store (cdr new-ref-store-pair)))
            (an-answer (ref-val new-ref) new-store)))

        (deref-exp (exp1)
          (let ((ans1 (value-of exp1 env store)))
            (let ((ref1 (expval->ref (answer->val ans1))))
              (an-answer (deref store ref1) store))))

        (setref-exp (exp1 exp2)
          (let* ((ans1 (value-of exp1 env store))
                 (ref (expval->ref (answer->val ans1)))
                 (ans2 (value-of exp2 env (answer->store ans1)))
                 (val2 (answer->val ans2))
                 (new-store (setref (answer->store ans2) ref val2)))
            (an-answer
              (num-val 23)
              new-store)))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg store)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list store)))
                  (eopl:printf "~%")))
              (value-of body new-env store)))))))


  ; (trace value-of)
  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
