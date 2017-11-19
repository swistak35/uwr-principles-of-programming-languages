(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

  (require (only-in racket/list
                    range))
  (require (only-in racket/vector
                    vector-map))

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

  (define (rec-proc? p)
    (and (symbol? (car p) (vector? (cadr p)))))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec-*
      (rec-procs vector?)
      (saved-env environment?)))

  (require racket/trace)

  (define build-extend-env-rec-*
    (lambda (rec-procs saved-env)
      (letrec ((vec (make-vector (length rec-procs)))
               (new-env (extend-env-rec-* vec saved-env)))
        (for-each
          (lambda (i rp)
            (vector-set! vec i
                         (cons
                           (car rp)
                           (proc-val (procedure (cadr rp) (caddr rp) new-env)))))
          (range (vector-length vec))
          rec-procs)
        new-env)))

  ; (trace build-extend-env-rec-*)

)
