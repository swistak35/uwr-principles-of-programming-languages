#lang eopl

(require rackunit)
(require "data-structures.scm")
(require "lang.scm")
(require "interp.rkt")

(provide interp-tests)

(define (expval->val val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (list-val (lst) (map expval->val lst))
    (else
      (eopl:error 'expval->val "Can't convert expval to val: ~s" val))))

(define (runner prg)
  (expval->val (value-of-program (scan&parse prg))))

(define (runner-d prg)
  (lambda () (runner prg)))

(define interp-tests
  (test-suite
    "Interpreter"

    (test-equal?
      "Positive constant"
      (runner "12")
      12)

    (test-equal?
      "negative-const"
      (runner "-33")
      -33)

    (test-equal?
      "primitive diff simple-arith-1"
      (runner "(diff 44 33)")
      11)

    (test-equal?
      "primitive diff nested-arith-left"
      (runner "(diff (diff 44 33) 22)")
      -11)

    (test-equal?
      "primitive diff nested-arith-right"
      (runner "(diff 55 (diff 22 11))")
      44)

    (test-exn
      "primitive call with too many args"
      #rx"Wrong number of arguments to diff, given 3 expected 2"
      (runner-d "(diff 1 2 3)"))

    (test-exn
      "primitive call with too little args"
      #rx"Wrong number of arguments to diff, given 1 expected 2"
      (runner-d "(diff 1)"))

    (test-exn
      "primitive call with no args"
      #rx"Wrong number of arguments to diff, given 0 expected 2"
      (runner-d "(diff)"))

    (test-exn
      "test-unbound-var-1"
      #rx"apply-env"
      (runner-d "foo"))

    (test-exn
      "test-unbound-var-2"
      #rx"apply-env"
      (runner-d "(diff x foo)"))

    (test-equal?
      "if-true"
      (runner "if (zero? 0) then 3 else 4")
      3)

    (test-equal?
      "if-false"
      (runner "if (zero? 1) then 3 else 4")
      4)

    (test-exn
      "no-bool-to-diff-1"
      #rx"expval-extractors"
      (runner-d "(diff (zero? 0) 1)"))

    (test-exn
      "no-bool-to-diff-2"
      #rx"expval-extractors"
      (runner-d "(diff 1 (zero? 0))"))

    (test-exn
      "no-int-to-if"
      #rx"expval-extractors"
      (runner-d "if 1 then 2 else 3"))

    (test-equal?
      "if-eval-test-true"
      (runner "if (zero? (diff 11 11)) then 3 else 4")
      3)

    (test-equal?
      "if-eval-test-false"
      (runner "if (zero? (diff 11 12)) then 3 else 4")
      4)

    (test-equal?
      "if-eval-test-true-2"
      (runner "if (zero? (diff 11 11)) then 3 else foo")
      3)

    (test-equal?
      "if-eval-test-false-2"
      (runner "if (zero? (diff 11 12)) then foo else 4")
      4)

    (test-equal?
      "simple-let-1"
      (runner "let x = 3 in x")
      3)

    (test-equal?
      "eval-let-body"
      (runner "let x = 3 in (diff x 1)")
      2)

    (test-equal?
      "eval-let-rhs"
      (runner "let x = (diff 4 1) in (diff x 1)")
      2)

    (test-equal?
      "simple-nested-let"
      (runner "let x = 3 in let y = 4 in (diff x y)")
      -1)

    (test-equal?
      "check-shadowing-in-body"
      (runner "let x = 3 in let x = 4 in x")
      4)

    (test-equal?
      "check-shadowing-in-rhs"
      (runner "let x = 3 in let x = (diff x 1) in x")
      2)

    (test-equal?
      "apply-proc-in-rator-pos"
      (runner "(proc(x) (diff x 1)  30)")
      29)

    (test-exn
      "check number of arguments in custom procs"
      #rx"Wrong number of arguments"
      (runner-d "letrec foo(x,y) = 42 in (foo 7)"))

    (test-equal?
      "apply-simple-proc"
      (runner "let f = proc (x) (diff x 1) in (f 30)")
      29)

    (test-equal?
      "let-to-proc-1"
      (runner "(proc(f)(f 30)  proc(x)(diff x 1))")
      29)

    (test-equal?
      "nested-procs"
      (runner "((proc (x) proc (y) (diff x y)  5) 6)")
      -1)

    (test-equal?
      "nested-procs2"
      (runner "let f = proc(x) proc (y) (diff x y) in ((f (diff 10 5)) 6)")
      -1)

    (test-equal?
      "y-combinator-1"
      (runner "
              let fix = proc (f)
              let d = proc (x)
              proc (z) ((f (x x)) z)
              in proc (n) ((f (d d)) n)
              in let t4m = proc (f)
              proc(x)
              if (zero? x) then 0 else (diff (f (diff x 1)) -4)
              in let times4 = (fix t4m)
              in (times4 3)")
              12)

    (test-equal?
      "simple-letrec-1"
      (runner "letrec f(x) = (diff x 1) in (f 33)")
      32)

    (test-equal?
      "simple-letrec-2"
      (runner "letrec f(x) = if (zero? x)
                             then 0
                             else (diff (f (diff x 1)) -2)
              in (f 4)")
      8)

    (test-equal?
      "simple-letrec-3"
      (runner "let m = -5 
              in letrec f(x) = if (zero? x)
              then 0
              else (diff (f (diff x 1)) m)
              in (f 4)")
      20)

    (test-equal?
      "HO-nested-letrecs"
      (runner "letrec even(odd) = proc(x) if (zero? x) then 1 else (odd (diff x 1))
              in letrec odd(x)  = if (zero? x) then 0 else ((even odd) (diff x 1))
              in (odd 13)")
              1)

    (test-equal?
      "begin-test-1"
      (runner "begin 1; 2; 3 end")
      3)

    (test-equal?
      "gensym-test-1"
      (runner "
              let g = let counter = (newref 0) 
              in proc (dummy) let d = setref(counter, (diff deref(counter) -1))
              in deref(counter)
              in (diff (g 11) (g 22))")
      -1)

    (test-equal?
      "simple-store-test-1"
      (runner "let x = (newref 17) in deref(x)")
      17)

    (test-equal?
      "assignment-test-1"
      (runner "let x = (newref 17) 
              in begin setref(x,27); deref(x) end")
      27)

    (test-equal?
      "gensym-test-2"
      (runner "
              let g = let counter = (newref 0) 
              in proc (dummy)
              begin
              setref(counter, (diff deref(counter) -1));
              deref(counter)
              end
              in (diff (g 11) (g 22))")
      -1)

    (test-equal?
      "even-odd-via-set-1"
      (runner "
              let x = (newref 0)
              in letrec even(d) = if (zero? deref(x)) 
              then 1
              else let d = setref(x, (diff deref(x) 1))
              in (odd d)
              odd(d)  = if (zero? deref(x)) 
              then 0
              else let d = setref(x, (diff deref(x) 1))
              in (even d)
              in let d = setref(x,13)
              in (odd -100)")
      1)

    (test-equal?
      "show-allocation-1"
      (runner "
              let x = (newref 22)
              in let f = proc (z)
              let zz = (newref (diff z deref(x)))
              in deref(zz)
              in (diff (f 66) (f 55))")
      11)

    (test-equal?
      "chains-1"
      (runner "
              let x = (newref (newref 0))
              in begin 
              setref(deref(x), 11);
              deref(deref(x))
              end")
      11)

    (test-equal?
      "list-1"
      (runner "[1, 2, 3]")
      '(1 2 3))

    (test-equal?
      "list-2"
      (runner "[]")
      '())

    (test-equal?
      "cons-1"
      (runner "cons(42, [])")
      '(42))

    (test-equal?
      "cons-2"
      (runner "cons(42, cons(17, []))")
      '(42 17))

    (test-equal?
      "car-1"
      (runner "car(cons(42, cons(17, [])))")
      42)

    (test-equal?
      "car-2"
      (runner "let x = [1, 2, 3] in car(x)")
      1)

    ; Make this error more sane, not internal interpreter detail
    (test-exn
      "car-3"
      #rx"car"
      (runner-d "car([])"))

    ; Make this error more sane, not internal interpreter detail
    (test-exn
      "cdr-1"
      #rx"cdr"
      (runner-d "cdr([])"))

    (test-equal?
      "cdr-2"
      (runner "cdr([2,3])")
      '(3))

    (test-equal?
      "cdr-3"
      (runner "cdr([3,2,1])")
      '(2 1))

    (test-equal?
      "null-1"
      (runner "null?([])")
      #t)

    (test-equal?
      "null-2"
      (runner "null?([1])")
      #f)

    ; Same as above - not needed when type inference finished
    (test-exn
      "null-3"
      #rx"expval-extractors"
      (runner-d "null?(42)"))

    (test-equal?
      "map-1"
      (runner "
              letrec map(f) =
              letrec map2(xs) = if null?(xs) then [] else cons((f car(xs)), ((map f) cdr(xs)))
              in map2
              increment(n) = (diff n -1)
              in ((map increment) [-1,0,1,2])")
      '(0 1 2 3))

    (test-equal?
      "filter-1"
      (runner "
              letrec filter(f) =
              letrec filter2(xs) = if null?(xs)
              then []
              else if (f car(xs))
              then cons(car(xs), ((filter f) cdr(xs)))
              else ((filter f) cdr(xs))
              in filter2
              iszero(n) = (zero? n)
              in ((filter iszero) [0,1,2,0,1,2])")
      '(0 0))

    (test-equal?
      "map-multiarg-1"
      (runner "
              letrec map(f, xs) = if null?(xs) then [] else cons((f car(xs)), (map f cdr(xs)))
                     increment(n) = (diff n -1)
              in (map increment [-1,0,1,2])")
              '(0 1 2 3))

    ))
