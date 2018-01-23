(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require (only-in racket/base
                    foldl))

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  (define (initial-env)
    (extend-env
      'diff (proc-val (primitive 'diff))
      (extend-env
        'zero? (proc-val (primitive 'zero?))
        (init-env))))

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (initial-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (apply-env env var))

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
          (proc-val (procedure (list var) body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env)) rands)))
            (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

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
            (list-val exps-vals)))

        )))

  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((new-env (foldl
                           (lambda (var arg res-env) (extend-env var arg res-env))
                           saved-env
                           vars args)))
            (value-of body new-env)))
        (primitive (name)
          (apply-primitive name args)))))

  (define (apply-primitive name args)
    (cond
      ((eq? name 'diff)
       (num-val (- (expval->num (car args)) (expval->num (cadr args)))))

      ((eq? name 'zero?)
       (bool-val (zero? (expval->num (car args)))))

      (else (eopl:error 'apply-primitive "Unknown primitive ~s" name))))

  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
