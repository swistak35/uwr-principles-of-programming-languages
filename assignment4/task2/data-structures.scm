(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

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

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  ;; Page: 86
  ; (define-datatype environment environment?
  ;   (empty-env)
  ;   (extend-env 
  ;     (bvar symbol?)
  ;     (bval expval?)
  ;     (saved-env environment?))
  ;   (extend-env-rec
  ;     (id symbol?)
  ;     (bvar symbol?)
  ;     (body expression?)
  ;     (saved-env environment?)))
  (define environment? procedure?)

  ;; empty-env : () -> Env
  (define empty-env
    (lambda ()
      (lambda (search-var) 
        (report-no-binding-found search-var))))

  ;; extend-env : Var * Schemeval * Env -> Env
  (define extend-env
    (lambda (saved-var saved-val saved-env)
      (lambda (search-var)
        (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

  (define extend-env-rec
    (lambda (p-name b-var p-body saved-env)
      (letrec ((new-env (lambda (search-var)
                          (if (eqv? search-var p-name)
                            (proc-val (procedure b-var p-body new-env))
                            (apply-env saved-env search-var)))))
        new-env)))


  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))

  (define apply-env
    (lambda (env search-var) 
      (env search-var)))

)
