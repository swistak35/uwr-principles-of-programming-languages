#lang racket

;; An example from "Defunctionalization at Work" by Danvy and Nielsen

;; aux : (Int -> Int) -> Int
(define (aux f)
  (+ (f 1) (f 10)))

;; main : Int * Int * Bool -> Int
(define (main x y b)
  (* (aux (lambda (z) (+ x z)))
     (aux (lambda (z) (if b (+ y z) (- y z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; representation-idependent client code

(define (aux-int f)
  (+ (apply-int f 1) (apply-int f 10)))

(define (main-int x y b)
  (* (aux-int (constr-1-int x))
     (aux-int (constr-2-int b y))))

;; a procedural (higher-order) implementation

(define (constr-1-int x)
  (lambda (z) (+ x z)))

(define (constr-2-int b y)
  (lambda (z) (if b (+ y z) (- y z))))

(define (apply-int f z)
  (f z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; representation-idependent client code

(define (aux-def f)
  (+ (apply-def f 1) (apply-def f 10)))

(define (main-def x y b)
  (* (aux-def (constr-1-def x))
     (aux-def (constr-2-def b y))))

;; a data-structure (first-order) implementation
;; obtained by defunctionalization

(define (constr-1-def x)
  (list 'constr-1 x))
 
(define (constr-2-def b y)
  (list 'constr-2 b y))

(define (apply-def f z)
  (cond ((eqv? (car f) 'constr-1)
         (let ((x (cadr f))) (+ x z)))
        ((eqv? (car f) 'constr-2)
         (let ((b (cadr f))
               (y (caddr f)))
           (if b (+ y z) (- y z))))))