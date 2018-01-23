(module prettyprinter (lib "eopl.ss" "eopl")

  ; (require "data-structures.scm")
  ; (require "drscheme-init.scm")
  (require "lang.scm")
  ; (require (only-in racket/base
  ;                   format))
  ; (require (only-in racket/string
  ;                   string-join))
  (require "type-data-structures.rkt")
  (require "unification.rkt")
  
  (provide (all-defined-out))

  (define-datatype assumption-set assumption-set?
    (empty-aset)
    (extend-aset 
      (bvar symbol?)
      (btype type?)
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

  (define (infer/pgm pgm)
    (cases program pgm
      (a-program (exp1)
        (infer exp1))))

  (define (infer exp)
    (initialize-typevar-counter!)
    (let* ((exp-ans (infer-exp exp (empty-aset)))
           (exp-type (answer->type exp-ans))
           (exp-subst (answer->subst exp-ans)))
      exp-type))

  (define (unify/one left right)
    (unify (list (an-equality left right)) (empty-subst)))

  (define (subst-in-aset subst aset)
    (cases assumption-set aset
      (empty-aset () aset)
      (extend-aset (bvar btype saved-aset)
        (extend-aset bvar (subst-in-type subst btype) (subst-in-aset subst saved-aset)))))

  ; Assumption: arg-type and return-type are concrete
  (define (handle-unary arg-type return-type exp aset)
    (let* ((fun-type (arrow-type arg-type return-type))
           (exp-answer (infer-exp exp aset))
           (exp-subst (answer->subst exp-answer))
           (unify-subst (unify/one
                          fun-type
                          (arrow-type (answer->type exp-answer) return-type))))
      (an-answer
        return-type
        (merge-subst exp-subst unify-subst))))

  (define (infer-exp exp aset)
    (cases expression exp
      (const-exp (num)
        (an-answer
          (int-type)
          (empty-subst)))

      (zero?-exp (exp1)
        (handle-unary (int-type) (bool-type) exp1 aset))

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

      (diff-exp (exp1 exp2)
        (let* ((exp1-answer (infer-exp exp1 aset))
               (exp2-aset-subst (merge-subst (answer->subst exp1-answer) (unify/one (int-type) (answer->type exp1-answer))))
               (exp2-answer (infer-exp exp2 (subst-in-aset exp2-aset-subst aset)))
               (final-subst (merge-subst (merge-subst exp2-aset-subst (answer->subst exp2-answer)) (unify/one (int-type) (answer->type exp2-answer)))))
          (an-answer
            (int-type)
            final-subst)))

      (proc-exp (bvar body)
        (let* ((arg-type (get-fresh-typevar))
               (body-answer (infer-exp body (extend-aset bvar arg-type aset))) ; bvar should be removed, but it's "overriden" so maybe it's ok
               (body-subst (answer->subst body-answer)))
          (an-answer
            (subst-in-type body-subst (arrow-type arg-type (answer->type body-answer)))
            body-subst)))

      (var-exp (var)
        (an-answer
          (apply-aset aset var)
          (empty-subst)))

      (call-exp (rator rands)
                ; does handle only one argument procedures now
        (let* ((rand (car rands))
               (rator-answer (infer-exp rator aset))
               (rand-aset-subst (answer->subst rator-answer))
               (rand-answer (infer-exp rand (subst-in-aset rand-aset-subst aset)))
               (result-typevar (get-fresh-typevar))
               (final-subst (merge-subst (merge-subst rand-aset-subst (answer->subst rand-answer)) (unify/one
                                                                                                     (subst-in-type (answer->subst rand-answer) (answer->type rator-answer))
                                                                                                     (arrow-type (answer->type rand-answer) result-typevar)))))
          (an-answer
            (subst-in-type final-subst result-typevar) ; in theory it should be only unificator from unify/one above?
            final-subst)))

      (else (eopl:error 'infer "Unhandled expression ~s" exp))
      ))

  )
