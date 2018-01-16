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

      ; ;; make sure the body and rhs get evaluated
      ; (eval-let-body "let x = 3 in -(x,1)" 2)
      ; (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ; ;; check nested let and shadowing
      ; (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      ; (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      ; (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ; ;; simple applications
      ; (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      ; (apply-simple-proc "let f = proc (x) -(x,1)
      ;                    in (f 30)" 29)
      ; (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)

      ; (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      ; (nested-procs2 "let f = proc(x) proc (y) -(x,y)
      ;                in ((f -(10,5)) 6)" -1)

      )))
