#lang eopl

(require rackunit)
(require "lang.scm")
(require "prettyprinter.scm")

(provide prettyprinter-tests)

(define (runner prg)
  (pretty-print/pgm (scan&parse prg)))

(define prettyprinter-tests
  (test-suite
    "Pretty printer"

    (test-equal?
      "Positive constant"
      (runner "12")
      "12")

    (test-equal?
      "Negative constant"
      (runner "-12")
      "-12")

    (test-equal?
      "Simple arithmethic"
      (runner "-(44,33)")
      "-(44,33)")

    (test-equal?
      "Nested airth left"
      (runner "-(-(44,33),22)")
      "-(-(44,33),22)")

    (test-equal?
      "Nested airth left"
      (runner "-(22,-(44,33))")
      "-(22,-(44,33))")

    (test-equal?
      "Var exp"
      (runner "x")
      "x")

    (test-equal?
      "If exp"
      (runner "if 1 then 2 else 3")
"if 1
then 2
else 3")

    (test-equal?
      "zero? exp"
      (runner "zero?(42)")
      "zero?(42)")

    (test-equal?
      "let"
      (runner "let x = 3 in x")
"let x = 3
in x")

    (test-equal?
      "proc"
      (runner "proc (y) -(42, y)")
      "proc(y) -(42,y)")

    (test-equal?
      "call"
      (runner "(f 42)")
      "(f 42)")

    (test-equal?
      "letrec"
      (runner "letrec foo(x) = -(42,x)
                      bar(y) = -(y,z) in (foo (bar 17))")
"letrec foo(x) = -(42,x)
       bar(y) = -(y,z)
in (foo (bar 17))")

    (test-equal?
      "begin"
      (runner "begin (f 17); (g 42); (h -123) end")
"begin
  (f 17);
  (g 42);
  (h -123)
end")

    (test-equal?
      "newref"
      (runner "newref(42)")
      "newref(42)")

    (test-equal?
      "deref"
      (runner "deref(y)")
      "deref(y)")

    (test-equal?
      "setref"
      (runner "setref(y, 42)")
      "setref(y, 42)")

    (test-equal?
      "empty list"
      (runner "[]")
      "[]")

    (test-equal?
      "singleton"
      (runner "[42]")
      "[42]")

    (test-equal?
      "longer list"
      (runner "[42, 0]")
      "[42, 0]")

    (test-equal?
      "car"
      (runner "car([42, 17])")
      "car([42, 17])")

    (test-equal?
      "cdr"
      (runner "cdr([42, 17])")
      "cdr([42, 17])")

    (test-equal?
      "null?"
      (runner "null?([42, 17])")
      "null?([42, 17])")

    (test-equal?
      "cons"
      (runner "cons(63, [42, 17])")
      "cons(63, [42, 17])")

    ))