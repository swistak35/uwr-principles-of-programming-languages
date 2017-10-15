#lang racket

; I wasn't using eopl because it wasn't displaying the results by default and atom? was undefined there anyway

; Task 1 (1.21)
(define product
  (lambda (sos1 sos2) (product2 sos2 sos1 sos2 '())))

(define product2
  (lambda (sos2 sos1a sos2a acc)
    (cond
      ((null? sos1a) acc)
      ((null? sos2a) (product2 sos2 (cdr sos1a) sos2 acc))
      (else (product2 sos2 sos1a (cdr sos2a) (cons (cons (car sos1a) (cons (car sos2a) '())) acc))))))

(product '(a b c) '(x y))

; Task 2 (1.26)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define up
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst)) (cons (car lst) (up (cdr lst))))
      (else (append (car lst) (up (cdr lst)))))))

(up '((1 2) (3 4)))
(up '((x (y)) z))

; Task 3 (1.28)

(define (merge loi1 loi2)
  (cond
    ((null? loi1) loi2)
    ((null? loi2) loi1)
    ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
    (else (cons (car loi2) (merge loi1 (cdr loi2))))))

(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))



