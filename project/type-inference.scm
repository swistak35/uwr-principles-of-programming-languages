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
    (var-type
      (id integer?)))

  (define typevar-counter 'uninitialized)
  (define (initialize-typevar-counter!)
    (set! typevar-counter 0))
  (define (get-fresh-typevar-id)
    (set! typevar-counter (+ typevar-counter 1))
    typevar-counter)
  (define (get-fresh-typevar)
    (var-type (get-fresh-typevar-id)))

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

  (define-datatype substitution substitution?
    (empty-subst)
    (extend-subst 
      (varid integer?)
      (stype type?)
      (saved-subst substitution?)))
  (define (apply-subst subst search-id)
    (cases substitution subst
      (empty-subst ()
        (var-type search-id))
      (extend-subst (id stype saved-subst)
        (if (eqv? id search-id)
          stype
          (apply-subst saved-subst search-id)))))

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

  (define (unify equalities subst)
    (if (null? equalities)
      subst
      (let* ((cur-eq (car equalities))
             (cur-eq-left (eq->left cur-eq))
             (cur-eq-right (eq->right cur-eq)))
        (cases type cur-eq-left
          (var-type (id1)
            (cases type cur-eq-right
              (int-type ()
                (unify (cdr equalities) (extend-subst id1 (int-type) subst)))
              (else
                (if (equal? cur-eq-left cur-eq-right)
                  (unify (cdr equalities) subst)
                  (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))
          (int-type ()
            (cases type cur-eq-right
              (var-type (id1)
                (unify (cdr equalities) (extend-subst id1 (int-type) subst)))
              (else
                (if (equal? cur-eq-left cur-eq-right)
                  (unify (cdr equalities) subst)
                  (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))
          (else
            (if (equal? cur-eq-left cur-eq-right)
              (unify (cdr equalities) subst)
              (eopl:error 'infer "Unification of equality ~s failed with substitution ~s" cur-eq subst)))))))

  (define (subst-in-type subst stype)
    (cases type stype
      (int-type ()
        (int-type))
      (bool-type ()
        (bool-type))
      (arrow-type (left right)
        (arrow-type (subst-in-type subst left) (subst-in-type subst right)))
      (var-type (id)
        (apply-subst subst id))
      (else eopl:error 'subst-in-type "Unhandled type ~s" stype)))

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
