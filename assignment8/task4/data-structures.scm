(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; list of expvals.

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
      (var symbol?)
      (handler-exp expression?)
      (env environment?)
      (cont continuation?))
    (raise1-cont
      (saved-cont continuation?))
    )

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (cont-val
      (cont continuation?))
    (list-val
      (lst (list-of expval?))))

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->cont
    (lambda (v)
      (cases expval v
	     (cont-val (cont) cont)
	     (else (expval-extractor-error 'cont v)))))

  (define expval->list
    (lambda (v)
      (cases expval v
	(list-val (lst) lst)
	(else (expval-extractor-error 'list v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;; ;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

;; moved to interp.scm

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; replaced by custom environment structure in environments.scm.
;;; This represents an environment as an alist  ((id rhs) ...)
;;; where rhs is either an expval or a list (bvar body)
;;; expval is for extend-env; the list is for extend-env-rec.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

;;; The code for this is in environments.scm, but we need environment?
;;; for define-datatype proc, so we write an appoximation:

  (define environment?
    (list-of
      (lambda (p)
        (and 
          (pair? p)
          (symbol? (car p))))))

  )
