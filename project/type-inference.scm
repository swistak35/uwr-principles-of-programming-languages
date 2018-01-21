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
      (answer-constraints (list-of equality?))))

  (define (answer->type ans)
    (cases answer ans
      (an-answer (ans-type ans-constraints) ans-type)))

  (define (answer->constraints ans)
    (cases answer ans
      (an-answer (ans-type ans-constraints) ans-constraints)))

  (define (infer/pgm pgm)
    (cases program pgm
      (a-program (exp1)
        (infer exp1))))

  (define (infer exp)
    (initialize-typevar-counter!)
    (let* ((exp-ans (infer-exp exp (empty-aset)))
           (exp-type (answer->type exp-ans))
           (exp-constraints (answer->constraints exp-ans))
           (exp-subst (unify exp-constraints (empty-subst))))
      (subst-in-type exp-subst exp-type)))

  (define (infer-exp exp aset)
    (cases expression exp
      (const-exp (num)
        (an-answer
          (int-type)
          '()))

      (zero?-exp (exp1)
        (let ((exp1-answer (infer-exp exp1 aset)))
          (an-answer
            (bool-type)
            (cons
              (an-equality (int-type) (answer->type exp1-answer))
              (answer->constraints exp1-answer)))))

      (if-exp (exp1 exp2 exp3)
        (let ((exp1-answer (infer-exp exp1 aset))
              (exp2-answer (infer-exp exp2 aset))
              (exp3-answer (infer-exp exp3 aset)))
          (an-answer
            (answer->type exp2-answer)
            (append
              (list
                (an-equality (bool-type) (answer->type exp1-answer))
                (an-equality (answer->type exp2-answer) (answer->type exp3-answer)))
              (answer->constraints exp1-answer)
              (answer->constraints exp2-answer)
              (answer->constraints exp3-answer)))))

      (diff-exp (exp1 exp2)
        (let ((exp1-answer (infer-exp exp1 aset))
              (exp2-answer (infer-exp exp2 aset)))
          (an-answer
            (int-type)
            (append
              (list
                (an-equality (int-type) (answer->type exp1-answer))
                (an-equality (int-type) (answer->type exp2-answer)))
              (answer->constraints exp1-answer)
              (answer->constraints exp2-answer)))))

      (proc-exp (bvar body)
        (let* ((arg-type (get-fresh-typevar))
               (body-answer (infer-exp body (extend-aset bvar arg-type aset))))
          (an-answer
            (arrow-type arg-type (answer->type body-answer))
            (answer->constraints body-answer))))

      (var-exp (var)
        (an-answer
          (apply-aset aset var)
          '()))

      (else (eopl:error 'infer "Unhandled expression ~s" exp))
      ))

  )
