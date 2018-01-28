#lang eopl

(require rackunit)
(require "lang.rkt")
(require "type-data-structures.rkt")
(require "inference.rkt")
(require "prettyprint-type.rkt")

(provide inference-tests)

(define (runner prg)
  (prettyprint-type (infer-program (scan&parse prg))))

(define (runner-d prg)
  (lambda () (runner prg)))

(define inference-tests
  (test-suite
    "Type inference"

    ;;; Basic syntax

    (test-equal?
      "Positive constants"
      (runner "12")
      "int")

    (test-equal?
      "if correct"
      (runner "if (zero? 42) then 1 else 2")
      "int")

    (test-exn
      "if with incorrect condition type"
      #rx"Unification"
      (runner-d "if 42 then 1 else 2"))

    (test-exn
      "if with different result types"
      #rx"Unification"
      (runner-d "if (zero? 42) then (zero? 1) else 2"))

    (test-exn
      "unbound variable"
      #rx"No binding"
      (runner-d "x"))

    (test-equal?
      "simple begin"
      (runner "begin 42 end")
      "int")

    (test-equal?
      "simple begin 2"
      (runner "begin (zero? 42); 17 end")
      "int")

    (test-equal?
      "empty list"
      (runner "[]")
      "'1 list")

    (test-equal?
      "simple list"
      (runner "[42]")
      "int list")

    (test-equal?
      "list of ints"
      (runner "[42, 17]")
      "int list")

    (test-equal?
      "list of bools 1"
      (runner "[(zero? 0), (zero? 42)]")
      "bool list")

    (test-exn
      "heterogenous list"
      #rx"Unification"
      (runner-d "[42, (zero? 42)]"))

    (test-equal?
      "list of lists"
      (runner "[[42], [17]]")
      "int list list")

    (test-equal?
      "list of lists"
      (runner "[[], [17]]")
      "int list list")

    (test-equal?
      "list of lists"
      (runner "[[17], []]")
      "int list list")

    (test-equal?
      "list of lists of lists"
      (runner "[[[42]], [[17]]]")
      "int list list list")

    (test-exn
      "heterogenous list of lists"
      #rx"Unification"
      (runner-d "[[42], [(zero? 17)]]"))

    ;;; Primitives

    (test-equal?
      "primitive diff"
      (runner "diff")
      "(int * int) -> int")

    (test-equal?
      "primitive zero?"
      (runner "zero?")
      "int -> bool")

    (test-equal?
      "primitive newref"
      (runner "newref")
      "'1 -> '1 ref")

    (test-equal?
      "primitive deref"
      (runner "deref")
      "'1 ref -> '1")

    (test-equal?
      "primitive setref"
      (runner "setref")
      "('1 ref * '1) -> int")

    (test-equal?
      "primitive cons"
      (runner "cons")
      "('1 * '1 list) -> '1 list")

    (test-equal?
      "primitive car"
      (runner "car")
      "'1 list -> '1")

    (test-equal?
      "primitive cdr"
      (runner "cdr")
      "'1 list -> '1 list")

    (test-equal?
      "primitive null?"
      (runner "null?")
      "'1 list -> bool")

    ;;; Calls

    (test-equal?
      "diff correct"
      (runner "(diff 42 22)")
      "int")

    (test-exn
      "diff incorrect 1st arg"
      #rx"Unification"
      (runner-d "(diff (zero? 42) 22)"))

    (test-exn
      "diff incorrect 2nd arg"
      #rx"Unification"
      (runner-d "(diff 22 (zero? 42))"))

    (test-equal?
      "simple proc"
      (runner "proc(y) (diff 42 y)")
      "int -> int")

    (test-equal?
      "simple proc 2"
      (runner "proc(y) (zero? y)")
      "int -> bool")

    (test-equal?
      "simple polymorphic proc"
      (runner "proc(y) 42")
      "'1 -> int")

    (test-equal?
      "zero? with int"
      (runner "(zero? 42)")
      "bool")

    (test-exn
      "zero? with bool"
      #rx"Unification"
      (runner-d "(zero? (zero? 42))"))

    (test-equal?
      "simple call"
      (runner "(proc(y) (diff 42 y) 20)")
      "int")

    (test-exn
      "call with wrong argument"
      #rx"Unification"
      (runner-d "(proc(y) (diff 42 y) (zero? 42))"))

    (test-exn
      "call to something which is not a function"
      #rx"Unification"
      (runner-d "(42 42)"))

    (test-equal?
      "using polymorphic primitive"
      (runner "(car [1])")
      "int")

    (test-equal?
      "example not using type scheme of primitive"
      (runner "begin (car [1]); (car [2]) end")
      "int")

    (test-equal?
      "example using type scheme of primitive"
      (runner "begin (car [(zero? 1)]); (car [42]) end")
      "int")

    (test-equal?
      "example using type scheme of procedure"
      (runner "let double = proc(f) proc(x) (f (f x)) in ((double cdr) [17, 42])")
      "int list")

    (test-equal?
      "example polymoprhic using of procedure"
      (runner "let double = proc(f) proc(x) (f (f x)) in begin ((double cdr) [17, 42]); ((double cdr) [(zero? 17)]) end")
      "bool list")

    (test-equal?
      "example when we need to constrain variable set, simple"
      (runner "proc(f) proc(x) let g = f in (g x)")
      "('2 -> '3) -> '2 -> '3"
      )

    (test-exn
      "example when we need to constraint variable set"
      #rx"Unification"
      (runner-d "((proc(f) proc(x) let g = f in (g 0)  proc(x) if x then (zero? 0) else (zero? 42)) (zero? 0))"))

    (test-equal?
      "simple letrec example"
      (runner "letrec counter(n) = if (zero? n) then 42 else (counter (diff n 1)) in counter")
      "int -> int")

    (test-equal?
      "simple letrec example execution"
      (runner "letrec counter(n) = if (zero? n) then 42 else (counter (diff n 1)) in (counter 13)")
      "int")

    (test-equal?
      "nested letrecs"
      (runner "letrec even(oddf) = proc(x) if (zero? x) then 1 else (oddf (diff x 1))
               in letrec odd(x)  = if (zero? x) then 0 else ((even odd) (diff x 1))
                  in (odd 13)")
      "int")

    (test-equal?
      "mutual procedure calling - even"
      (runner "letrec even(n) = if (zero? n) then (zero? 0) else (odd (diff n 1))
                      odd(n)  = if (zero? n) then (zero? 1) else (even (diff n 1))
               in even")
      "int -> bool")

    (test-equal?
      "mutual procedure calling - odd"
      (runner "letrec even(n) = if (zero? n) then (zero? 0) else (odd (diff n 1))
                      odd(n)  = if (zero? n) then (zero? 1) else (even (diff n 1))
               in odd")
      "int -> bool")

    (test-equal?
      "map definition"
      (runner "letrec map(f, xs) = if (null? xs) then [] else (cons (f (car xs)) (map f (cdr xs))) in map")
      "(('16 -> '15) * '16 list) -> '15 list")

    (test-equal?
      "map example"
      (runner "letrec map(f, xs) = if (null? xs) then [] else (cons (f (car xs)) (map f (cdr xs)))
                      increment(n) = (diff n -1)
               in (map increment [-1,0,1,2])")
      "int list")

    (test-equal?
      "map example no multiarg definition"
      (runner "letrec map(f) = letrec map2(xs) = if (null? xs) then [] else (cons (f (car xs)) ((map f) (cdr xs)))
                               in map2
                      increment(n) = (diff n -1)
               in map")
      "('21 -> '20) -> '21 list -> '20 list")

    (test-equal?
      "map example no multiarg"
      (runner "letrec map(f) = letrec map2(xs) = if (null? xs) then [] else (cons (f (car xs)) ((map f) (cdr xs)))
                               in map2
                      increment(n) = (diff n -1)
               in ((map increment) [-1,0,1,2])")
      "int list")

    (test-exn
      "we do not have polymorphism inside recursive definitions"
      #rx"Unification"
      (runner-d "letrec foo(x) = if (zero? 0) then 42 else begin (foo (zero? 0)); (foo 42); 42 end in foo"))

    (test-equal?
      "argument names override procedure names, 1"
      (runner "letrec foo(bar) = if bar then 42 else 17
                      bar(x) = x
               in foo")
      "bool -> int")

    (test-equal?
      "argument names override procedure names, 1 (order doesnt matter)"
      (runner "letrec bar(x) = x
                      foo(bar) = if bar then 42 else 17
               in foo")
      "bool -> int")

    (test-exn
      "calling with too little arguments in letrec body"
      #rx"Unification"
      (runner-d "letrec foo(x, y) = 42 in (foo 1)"))

    (test-exn
      "calling with too many arguments in letrec body"
      #rx"Unification"
      (runner-d "letrec foo(x, y) = 42 in (foo 1 2 3)"))

    (test-exn
      "calling with too many arguments in procedure body"
      #rx"Unification"
      (runner-d "letrec foo(x, y) = (foo 1 2 3) in 42"))

    (test-exn
      "calling with too little arguments in procedure body"
      #rx"Unification"
      (runner-d "letrec foo(x, y) = (foo 1) in 42"))

    (test-exn
      "calling with too little arguments in procedure body - dubious example"
      #rx"Unification failed - occurs free check"
      (runner-d "letrec foo(x, y) = (foo x) in foo"))

    (test-exn
      "testing whether unification algorithm checks for occurrence"
      #rx"Unification failed - occurs free check"
      (runner-d "proc(f) (zero? (f f))"))

    ; It is expected that this version of fixpoint operator does not typecheck
    (test-exn
      "fixpoint operator"
      #rx"Unification failed - occurs free check"
      (runner-d "let fix = proc (f) let d = proc (x) proc (z) ((f (x x)) z)
                                    in proc (n) ((f (d d)) n)
                 in let t4m = proc (f) proc(x) if (zero? x) then 0 else (diff (f (diff x 1)) -4)
                    in let times4 = (fix t4m)
                       in (times4 3)"))

    ;;; Explicit refs

    (test-equal?
      "gensym program"
      (runner "let g = let counter = (newref 0) 
                       in proc (dummy) let d = (setref counter (diff (deref counter) -1))
                                       in (deref counter)
               in (diff (g 11) (g 22))")
      "int")

    (test-equal?
      "simple deref test"
      (runner "let x = (newref 17) in (deref x)")
      "int")

    (test-equal?
      "simple newref test"
      (runner "let x = (newref 17) in x")
      "int ref")

    (test-exn
      "can't assign other type"
      #rx"Unification"
      (runner-d "let x = (newref 17) 
               in (setref x (zero? 0))"))

    (test-equal?
      "even-odd-via-set example"
      (runner "let x = (newref 0)
               in letrec even(d) = if (zero? (deref x)) then 1 else let d = (setref x (diff (deref x) 1))
                                                                    in (odd d)
                         odd(d) = if (zero? (deref x)) then 0 else let d = (setref x (diff (deref x) 1))
                                                                   in (even d)
                  in let d = (setref x 13)
                     in (odd -100)")
      "int")

    ;; This one is from the Pierce's book
    (test-exn
      "ref dangerous example"
      #rx"Unification"
      (runner-d "let r = (newref proc(x) x) in begin (setref r proc(x) (diff x 1)); ((deref r) (zero? 0)) end"))

    ;; This one additionally check that we are not overly eager
    (test-equal?
      "ref safe example"
      (runner "letrec newref2(x) = (newref x) in begin (newref2 42); (newref (zero? 42)) end")
      "bool ref")

    ;; Similarly as above, but a different edge case
    (test-equal?
      "ref another safe example"
      (runner "letrec newref2(x) = (newref proc(y) y) in begin
                                                          ((deref (newref2 42)) 78);
                                                          ((deref (newref2 (zero? 42))) (zero? 42))
                                                         end")
      "bool")

    ))
