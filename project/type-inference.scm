(module prettyprinter (lib "eopl.ss" "eopl")

  ; (require "data-structures.scm")
  ; (require "drscheme-init.scm")
  (require "lang.scm")
  ; (require (only-in racket/base
  ;                   format))
  ; (require (only-in racket/string
  ;                   string-join))
  
  (provide (all-defined-out))

  (define-datatype type type?
    (int-type)
    (bool-type)
    (list-type
      (elem type?))
    (arrow-type
      (left type?)
      (right type?))
    )

  (define-datatype equality equality?
    (an-equality
      (left type?)
      (right type?)))
  (define (eq->left eq)
    (cases equality eq
      (an-equality (eq-left eq-right) eq-left)))
  (define (eq->right eq)
    (cases equality eq
      (an-equality (eq-left eq-right) eq-right)))

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

  (define (unify equalities)
    (if (null? equalities)
      #t
      (let* ((cur-eq (car equalities))
             (cur-eq-left (eq->left cur-eq))
             (cur-eq-right (eq->right cur-eq)))
        (if (equal? cur-eq-left cur-eq-right)
          (unify (cdr equalities))
          #f))))

  (define (infer exp)
    (let* ((exp-ans (infer-exp exp))
           (exp-type (answer->type exp-ans))
           (exp-constraints (answer->constraints exp-ans)))
      (if (unify exp-constraints)
        exp-type
        (eopl:error 'infer "Unification of constraints failed ~s" exp-constraints))))

  (define (infer-exp exp)
    (cases expression exp
      (const-exp (num)
        (an-answer
          (int-type)
          '()))

      (zero?-exp (exp1)
        (let ((exp1-answer (infer-exp exp1)))
          (an-answer
            (bool-type)
            (cons
              (an-equality (int-type) (answer->type exp1-answer))
              (answer->constraints exp1-answer)))))

      (if-exp (exp1 exp2 exp3)
        (let ((exp1-answer (infer-exp exp1))
              (exp2-answer (infer-exp exp2))
              (exp3-answer (infer-exp exp3)))
          (an-answer
            (answer->type exp2-answer)
            (append
              (list
                (an-equality (bool-type) (answer->type exp1-answer))
                (an-equality (answer->type exp2-answer) (answer->type exp3-answer)))
              (answer->constraints exp1-answer)
              (answer->constraints exp2-answer)
              (answer->constraints exp3-answer)))))
      ; (var-exp ())
      (else (eopl:error 'infer "Unhandled expression ~s" exp))
      ))

  )
