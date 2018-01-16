(module tests mzscheme

  (require "lang.scm")

  (provide test-list)

  (define test-list
    `(
      (positive-const
        ,(const-exp 11)
        "11")

      (negative-const
        ,(const-exp -33)
        "-33")

      (simple-arith-1
        ,(diff-exp (const-exp 44) (const-exp 33))
        "-(44,33)")

      (nested-arith-left
        ,(diff-exp (diff-exp (const-exp 44) (const-exp 33)) (const-exp 22))
        "-(-(44,33),22)")

      (nested-arith-right
        ,(diff-exp (const-exp 55) (diff-exp (const-exp 22) (const-exp 11)))
        "-(55,-(22,11))")

      (test-var-1
        ,(var-exp 'x)
        "x")

      (test-unbound-var-1
        ,(var-exp 'foo)
        "foo")

      (if-1
        ,(if-exp (const-exp 1) (const-exp 2) (const-exp 3))
"if 1
then 2
else 3")

      (zero-1
        ,(zero?-exp (const-exp 13))
        "zero?(13)")

      (let-1
        ,(let-exp 'x (const-exp 3) (var-exp 'x))
"let x = 3
in x")

      (proc-val-1
        ,(proc-exp 'y (diff-exp (const-exp 42) (var-exp 'y)))
        "proc(y) -(42,y)")

      (call-1
        ,(call-exp (var-exp 'f) (const-exp 42))
        "(f 42)")

      (letrec-1
        ,(letrec-exp '(foo bar) '(x y)
                     (list
                       (diff-exp (const-exp 42) (var-exp 'x))
                       (var-exp 'y))
                     (call-exp (var-exp 'foo) (call-exp (var-exp 'bar) (const-exp 17))))
"letrec foo(x) = -(42,x)
       bar(y) = y
in (foo (bar 17))")

      (begin-1
        ,(begin-exp
           (call-exp (var-exp 'f) (const-exp 17))
           (list
             (call-exp (var-exp 'g) (const-exp 42))
             (call-exp (var-exp 'h) (const-exp -123))))
"begin
  (f 17);
  (g 42);
  (h -123)
end")

      (newref-1
        ,(newref-exp (const-exp 42))
        "newref(42)")

      (deref-1
        ,(deref-exp (var-exp 'y))
        "deref(y)")

      (setref-1
        ,(setref-exp (var-exp 'x) (const-exp 42))
        "setref(x, 42)")

      (list-1
        ,(list-exp (list))
        "[]")

      (list-2
        ,(list-exp (list (const-exp 17)))
        "[17]")

      (list-3
        ,(list-exp (list (const-exp 42) (const-exp 17)))
        "[42, 17]")

      (car-1
        ,(car-exp (list-exp (list (const-exp 42) (const-exp 17))))
        "car([42, 17])")

      (cdr-1
        ,(cdr-exp (list-exp (list (const-exp 42) (const-exp 17))))
        "cdr([42, 17])")

      (null?-1
        ,(null?-exp (list-exp (list (const-exp 42) (const-exp 17))))
        "null?([42, 17])")

      (cons-1
        ,(cons-exp (const-exp 63) (list-exp (list (const-exp 42) (const-exp 17))))
        "cons(63, [42, 17])")

      )))
