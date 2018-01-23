#lang eopl

(require rackunit)
(require "lang.rkt")
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
      "Primitive call"
      (runner "(diff 44 33)")
      "(diff 44 33)")

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
      "let"
      (runner "let x = 3 in x")
"let x = 3
in x")

    (test-equal?
      "proc"
      (runner "proc (y) (diff 42  y)")
      "proc(y) (diff 42 y)")

    (test-equal?
      "call"
      (runner "(f 42)")
      "(f 42)")

    (test-equal?
      "letrec"
      (runner "letrec foo(x) = (diff 42 x)
                      bar(y) = (diff y z) in (foo (bar 17))")
"letrec foo(x) = (diff 42 x)
       bar(y) = (diff y z)
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

    ))
