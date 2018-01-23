(module prettyprinter (lib "eopl.ss" "eopl")

  ; (require "data-structures.scm")
  ; (require "drscheme-init.scm")
  (require "lang.scm")
  (require (only-in racket/base
                    foldl))
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
    (handle-call (list arg-type) return-type (list exp) aset))

  ; Assumption: arg-types and return-type are concrete
  (define (handle-call arg-types return-type args aset)
    ; There should be an error if lengths of arg-types and args are not equal
    ; It doesn't handle passing 0 arguments either
    (let* ((args-type (if (= (length arg-types) 1)
                        (car arg-types)
                        (tuple-type arg-types)))
           (fun-type (arrow-type args-type return-type))
           (arg-result (foldl
                         (lambda (arg-type arg result)
                           (let* ((current-subst (car result))
                                  (current-aset (cadr result))
                                  (substituted-aset (subst-in-aset current-subst current-aset))
                                  (arg-answer (infer-exp arg substituted-aset))
                                  (next-subst
                                    (merge-subst
                                      (merge-subst current-subst (answer->subst arg-answer))
                                      (unify/one arg-type (answer->type arg-answer))))
                                  (next-aset (subst-in-aset next-subst current-aset)))
                             (list next-subst next-aset)))
                         (list (empty-subst) aset)
                         arg-types
                         args))
           (result-subst (car arg-result)))
      (an-answer
        (subst-in-type result-subst return-type)
        result-subst)))

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
        (handle-call
          (list (int-type) (int-type))
          (int-type)
          (list exp1 exp2)
          aset))

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
