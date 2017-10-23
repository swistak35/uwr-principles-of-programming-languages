#lang eopl

(require rackunit)

;; ZeroOrOne ::= 0 | 1
;; walk : Listof(ZeroOrOne) -> (Listof(ZeroOrOne) -> Bool) -> Bool
(define (walk1 xs k)
  (if (and (not (null? xs)) (eqv? (car xs) 0))
    (walk1 (cdr xs)
          (lambda (ys)
            (if (and (not (null? ys)) (eqv? (car ys) 1))
              (k (cdr ys))
              #f)))
    (k xs)))
;; main : Listof(ZeroOrOne) -> Bool
(define (main1 xs)
  (walk1 xs (lambda (ys) (null? ys))))

(check-false (main1 (list 0)))
(check-false (main1 (list 1)))
(check-true (main1 (list 0 1)))
(check-true (main1 (list 0 0 1 1)))
(check-false (main1 (list 0 1 1)))
(check-false (main1 (list 0 0 1)))
(check-false (main1 (list 0 0 1 1 0 1)))

; Defunctionalized

(define-datatype strategy strategy?
  (null-strategy)
  (other-strategy
    (old-strategy strategy?)))

(define (apply-strategy s ys)
  (cases strategy s
    (null-strategy ()
      (null? ys))
    (other-strategy (old-s)
      (if (and (not (null? ys)) (eqv? (car ys) 1))
        (apply-strategy old-s (cdr ys))
        #f))))

(define (walk2 xs k)
  (if (and (not (null? xs)) (eqv? (car xs) 0))
    (walk2 (cdr xs) (other-strategy k))
    (apply-strategy k xs)))

(define (main2 xs)
  (walk2 xs (null-strategy)))

(check-false (main2 (list 0)))
(check-false (main2 (list 1)))
(check-true (main2 (list 0 1)))
(check-true (main2 (list 0 0 1 1)))
(check-false (main2 (list 0 1 1)))
(check-false (main2 (list 0 0 1)))
(check-false (main2 (list 0 0 1 1 0 1)))
