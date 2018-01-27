#lang eopl

(require "lang.rkt")
(require (only-in racket/base
                  foldl printf))
(require (only-in racket/list
                  append* remove-duplicates))
(require (only-in racket/set
                  set-subtract))
(require "type-data-structures.rkt")
(require "unification.rkt")
(require "prettyprint-exp.rkt")
(require "type-prettyprint.rkt")

(provide (all-defined-out))

(define instrument-infer #f)

(define-datatype assumption-set assumption-set?
  (empty-aset)
  (extend-aset 
    (bvar symbol?)
    (btype type-scheme?)
    (saved-aset assumption-set?)))

(define (apply-aset aset var)
  (cases assumption-set aset
    (empty-aset ()
      (eopl:error 'apply-aset "No binding for ~s" var))
    (extend-aset (bvar btype saved-aset)
      (if (eqv? var bvar)
        btype
        (apply-aset saved-aset var)))))

(define-datatype answer answer?
  (an-answer
    (answer-type type?)
    (answer-subst substitution?)))

(define (answer->type ans)
  (cases answer ans
    (an-answer (ans-type ans-substitution) ans-type)))

(define (answer->subst ans)
  (cases answer ans
    (an-answer (ans-type ans-substitution) ans-substitution)))

(define primitives-types
  (list
    (list 'zero? (a-type-scheme '() (arrow-type
                                      (int-type)
                                      (bool-type))))
    (list 'diff (a-type-scheme '() (arrow-type
                                      (tuple-type (list (int-type) (int-type)))
                                      (int-type))))
    (list 'car (a-type-scheme (list 0) (arrow-type
                                          (list-type (var-type 0))
                                          (var-type 0))))
    (list 'cdr (a-type-scheme (list 0) (arrow-type
                                          (list-type (var-type 0))
                                          (list-type (var-type 0)))))
    ))

(define (initial-aset)
  (foldl
    (lambda (primitive aset)
      (let ((name (car primitive))
            (scheme (cadr primitive)))
        (extend-aset name scheme aset)))
    (empty-aset)
    primitives-types))

(define (infer/pgm pgm)
  (cases program pgm
    (a-program (exp1)
      (infer exp1))))

(define (infer exp)
  (initialize-typevar-counter!)
  (let* ((exp-ans (infer-exp exp (initial-aset)))
          (exp-type (answer->type exp-ans))
          (exp-subst (answer->subst exp-ans)))
    exp-type))

(define (unify/one left right)
  (unify (list (an-equality left right)) (empty-subst)))

(define (subst-in-aset subst aset)
  (cases assumption-set aset
    (empty-aset () aset)
    (extend-aset (bvar btypescheme saved-aset)
      (cases type-scheme btypescheme
        (a-type-scheme (quantified-ids quantified-type)
          (extend-aset
            bvar
            (a-type-scheme quantified-ids (subst-in-type subst quantified-type))
            (subst-in-aset subst saved-aset)))))))

(define (handle-call/maybe-arrow proclike-type args aset)
  (let* ((args-result (foldl
                        (lambda (arg result)
                          (let* ((current-subst (car result))
                                  (current-aset (cadr result))
                                  (current-arg-types (caddr result))
                                  (arg-answer (infer-exp arg current-aset))
                                  (next-subst (merge-subst current-subst (answer->subst arg-answer)))
                                  (next-aset (subst-in-aset next-subst current-aset))
                                  (next-arg-types (append current-arg-types (list (answer->type arg-answer)))))
                            (list next-subst next-aset next-arg-types)))
                        (list (empty-subst) aset (list))
                        args))
          (result-subst (car args-result))
          (raw-arg-types (caddr args-result))
          (arg-type (if (= 1 (length raw-arg-types))
                      (car raw-arg-types)
                      (tuple-type raw-arg-types)))
          (result-typevar (get-fresh-typevar))
          (unify-subst (unify/one
                        (arrow-type arg-type result-typevar)
                        (subst-in-type result-subst proclike-type)))
          (final-subst (merge-subst result-subst unify-subst))
          (final-type (subst-in-type final-subst result-typevar)))
    (an-answer final-type final-subst)))

(define (replace-typevars typ mapping)
  (cases type typ
    (var-type (id)
      (if (assoc id mapping)
        (cadr (assoc id mapping))
        (var-type id)))
    (int-type () (int-type))
    (bool-type () (bool-type))
    (arrow-type (left right)
      (arrow-type
        (replace-typevars left mapping)
        (replace-typevars right mapping)))
    (list-type (elem)
      (list-type (replace-typevars elem mapping)))
    (ref-type (elem)
      (ref-type (replace-typevars elem mapping)))
    (tuple-type (elems)
      (tuple-type (map (lambda (t) (replace-typevars t mapping)) elems)))
    ))

(define (instantiate tscheme)
  (cases type-scheme tscheme
    (a-type-scheme (quantified-ids quantified-type)
      (let* ((fresh-typevars (map (lambda (id) (get-fresh-typevar)) quantified-ids))
              (mapping (map list quantified-ids fresh-typevars)))
        (replace-typevars quantified-type mapping)))))

(define (infer-exp exp aset)
  (cases expression exp
    (const-exp (num)
      (an-answer
        (int-type)
        (empty-subst)))

    (if-exp (exp1 exp2 exp3)
      (let* ((exp1-answer (infer-exp exp1 aset))
              (exp1-subst (answer->subst exp1-answer))
              (exp1-type (answer->type exp1-answer))
              (exp2-aset-subst (merge-subst exp1-subst (unify/one (bool-type) exp1-type)))

              (exp2-aset (subst-in-aset exp2-aset-subst aset))
              (exp2-answer (infer-exp exp2 exp2-aset))
              (exp2-subst (answer->subst exp2-answer))
              (exp2-type (answer->type exp2-answer))
              (exp3-aset-subst (merge-subst exp2-aset-subst exp2-subst))

              (exp3-aset (subst-in-aset exp3-aset-subst aset))
              (exp3-answer (infer-exp exp3 exp3-aset))
              (exp3-subst (answer->subst exp3-answer))
              (exp3-type (answer->type exp3-answer))
              (final-subst (merge-subst (merge-subst exp3-aset-subst exp3-subst) (unify/one exp2-type exp3-type))))
        (an-answer
          (subst-in-type final-subst exp2-type)
          final-subst)))

    (proc-exp (bvar body)
      (let* ((arg-type (get-fresh-typevar))
              (body-answer (infer-exp body (extend-aset bvar (a-type-scheme '() arg-type) aset))) ; bvar should be removed, but it's "overriden" so maybe it's ok
              (body-subst (answer->subst body-answer)))
        (an-answer
          (subst-in-type body-subst (arrow-type arg-type (answer->type body-answer)))
          body-subst)))

    (var-exp (var)
      (let* ((var-scheme (apply-aset aset var)))
        (an-answer
          (instantiate var-scheme)
          (empty-subst))))

    (call-exp (rator rands)
      (let* ((rator-answer (infer-exp rator aset))
              (handle-call-answer (handle-call/maybe-arrow
                                    (answer->type rator-answer)
                                    rands
                                    (subst-in-aset (answer->subst rator-answer) aset)))
              (final-type (answer->type handle-call-answer))
              (final-subst (merge-subst (answer->subst rator-answer) (answer->subst handle-call-answer))))
        (print-instrument-infer exp final-type final-subst)
        (an-answer final-type final-subst)))

    (begin-exp (exp1 exps)
      (let ((result (foldl
                      (lambda (cexp res)
                        (let* ((current-subst (car res))
                                (current-aset (cadr res))
                                (cexp-answer (infer-exp cexp current-aset))
                                (final-subst (merge-subst current-subst (answer->subst cexp-answer))))
                          (list
                            final-subst
                            (subst-in-aset final-subst current-aset)
                            (answer->type cexp-answer))))
                      (list (empty-subst) aset 'no-type)
                      (cons exp1 exps))))
        (an-answer
          (caddr result)
          (car result))))

    (list-exp (exps)
      (let* ((elem-vartype (get-fresh-typevar))
              (result (foldl
                        (lambda (cexp res)
                          (let* ((current-subst (car res))
                                (current-aset (cadr res))
                                (cexp-answer (infer-exp cexp current-aset))
                                (cexp-final-subst (merge-subst current-subst (answer->subst cexp-answer)))
                                (final-subst (merge-subst
                                                cexp-final-subst
                                                (unify/one
                                                  (subst-in-type cexp-final-subst elem-vartype)
                                                  (answer->type cexp-answer)))))
                            (list
                              final-subst
                              (subst-in-aset final-subst current-aset))))
                        (list (empty-subst) aset)
                        exps))
              (final-subst (car result))
              (final-type (subst-in-type final-subst (list-type elem-vartype))))
        (print-instrument-infer exp final-type final-subst)
        (an-answer final-type final-subst)))

    (let-exp (var exp1 body)
      (let* ((exp1-answer (infer-exp exp1 aset))
              (substituted-aset (subst-in-aset (answer->subst exp1-answer) aset))
              (tscheme (generalize (answer->type exp1-answer) substituted-aset))
              (new-aset (extend-aset var tscheme substituted-aset))
              (body-answer (infer-exp body new-aset))
              (final-subst (merge-subst (answer->subst exp1-answer) (answer->subst body-answer)))
              (final-type (answer->type body-answer)))
        (an-answer final-type final-subst)))

    (else (eopl:error 'infer "Unhandled expression ~s" exp))

    ))

(define (free-var-ids-in-type typ)
  (remove-duplicates (free-var-ids-in-type/aux typ)))
(define (free-var-ids-in-type/aux typ)
  (cases type typ
    (arrow-type (left right)
      (append (free-var-ids-in-type/aux left) (free-var-ids-in-type/aux right)))
    (list-type (elem)
      (free-var-ids-in-type/aux elem))
    (ref-type (elem)
      (free-var-ids-in-type/aux elem))
    (tuple-type (elems)
      (append* (map free-var-ids-in-type/aux elems)))
    (int-type () '())
    (bool-type () '())
    (var-type (id) (list id))))

(define (free-var-ids-in-aset aset)
  (remove-duplicates (free-var-ids-in-aset/aux aset)))
; Not optimal, but trivial to optimize
(define (free-var-ids-in-aset/aux aset)
  (cases assumption-set aset
    (empty-aset () '())
    (extend-aset (bvar btype saved-aset)
      (append (free-var-ids-in-tscheme btype) (free-var-ids-in-aset/aux saved-aset)))))

(define (free-var-ids-in-tscheme tscheme)
  (cases type-scheme tscheme
    (a-type-scheme (quantified-ids quantified-type)
      (set-subtract
        (free-var-ids-in-type quantified-type)
        quantified-ids))))

(define (generalize typ aset)
  (a-type-scheme
    (set-subtract
      (free-var-ids-in-type typ)
      (free-var-ids-in-aset aset))
    typ))

(define (print-instrument-infer exp typ subst)
  (when instrument-infer
    (printf
      "Inferred the type of:  ~a\n               to be:  ~a\n   with substitution:  ~a\n\n"
      (prettyprint-exp exp)
      (prettyprint-type typ)
      (prettyprint-subst subst))))
