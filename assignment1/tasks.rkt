#lang racket

; I wasn't using eopl because it wasn't displaying the results by default and atom? was undefined there anyway

(require racket/trace)
(require rackunit)

; Task 1 (1.21)
(define product
  (lambda (sos1 sos2) (product2 sos2 sos1 sos2 '())))

(define product2
  (lambda (sos2 sos1a sos2a acc)
    (cond
      ((null? sos1a) acc)
      ((null? sos2a) (product2 sos2 (cdr sos1a) sos2 acc))
      (else (product2 sos2 sos1a (cdr sos2a) (cons (cons (car sos1a) (cons (car sos2a) '())) acc))))))

(check-equal? (product '(a b c) '(x y)) '((c y) (c x) (b y) (b x) (a y) (a x)))
(check-equal? (product '() '()) '())
(check-equal? (product '(a) '()) '())
(check-equal? (product '() '(x)) '())

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

(check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (up '((x (y)) z)) '(x (y) z))

; Task 3 (1.28)

(define (merge loi1 loi2) (merge/predicate < loi1 loi2))
(define (merge/predicate pred loi1 loi2)
  (cond
    ((null? loi1) loi2)
    ((null? loi2) loi1)
    ((pred (car loi1) (car loi2)) (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
    (else (cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))))

(check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))

; Task 4 (1.30)

(define (one? lst)
  (cond
    ((null? (cdr lst)) #t)
    (else #f)))

(one? '(a))
(one? '(a b))

(define (split lst) (split/acc lst (cons '() '())))
(define (split/acc lst acc)
  (cond
    ((null? lst) acc)
    ((one? lst) (cons (cons (car lst) (car acc)) (cdr acc)))
    (else (split/acc (cddr lst) (cons (cons (car lst) (car acc)) (cons (cadr lst) (cdr acc)))))))

(split '())
(split '(a))
(split '(a b))
(split '(a b c d e f))

(define (sort/predicate pred loi)
  (cond
    ((null? loi) loi)
    ((one? loi) loi)
    (else (let* ([loi-splitted (split loi)]
                 [loi-first (car loi-splitted)]
                 [loi-second (cdr loi-splitted)])
            (merge/predicate pred (sort/predicate pred loi-first) (sort/predicate pred loi-second))))))

(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))

; Task 5 (1.34)

(define bst/val car)
(define bst/left cadr)
(define bst/right caddr)

; Not defined if x is not present in tree

(define (path x node)
  (cond
    ((< (bst/val node) x) (cons 'right (path x (bst/right node))))
    ((> (bst/val node) x) (cons 'left (path x (bst/left node))))
    (else '())))

; (trace path)
; (trace bst/val)
; (trace bst/right)
; (trace bst/left)

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))

; Task 6

(define (exp k)
  (cond
    ((= k 0) (lambda (n) 1))
    (else
      (let ([exp-sub1 (exp (- k 1))])
        (lambda (n) (* n (exp-sub1 n)))))))

((exp 3) 2)

; Task 7

(define fact
  (let ((f (lambda (g)
              (lambda (n)
                (if (= n 0)
                  1
                  (* n ((g g) (- n 1))))))))
    (f f)))

(fact 5)

; Task 8

(define (fix maker)
  (let ((g (lambda (x) (lambda (n) ((maker (x x)) n)))))
    (g g)))

(define fact-maker
  (trace-lambda (h)
    (trace-lambda (n)
      (if (= n 0)
        1
        (* n (h (- n 1)))))))

(define fact2
  (fix fact-maker))

;(trace fix)
;(trace fact-maker)
;(trace fact2)

(fact2 5)

; Task 9

; (define (halts-for-nil lst) ...)

; (define (loops-forever x) (loops-forever x))
; (loops-forever 42)
; (define (contradiction lst)
;   (if (halts-for-nil contradiction)
;     (if (null? lst)
;       (loops-forever 42)
;       42)
;     (if (null? lst)
;       42
;       (loops-forever 42))))
