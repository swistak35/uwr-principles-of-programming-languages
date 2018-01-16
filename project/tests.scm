(module tests mzscheme
  
  (provide test-list)
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1)
                            in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)

      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y)
                        in ((f -(10,5)) 6)" -1)
      
       (y-combinator-1 "
          let fix = proc (f)
                      let d = proc (x)
                                proc (z) ((f (x x)) z)
                        in proc (n) ((f (d d)) n)
            in let t4m = proc (f)
                           proc(x)
                             if zero?(x) then 0 else -((f -(x,1)),-4)
                 in let times4 = (fix t4m)
                      in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec f(x) = if zero?(x)
                       then 0
                       else -((f -(x,1)), -2)
           in (f 4)" 8)

      (simple-letrec-3
        "let m = -5 
           in letrec f(x) = if zero?(x)
                            then 0
                            else -((f -(x,1)), m)
                in (f 4)" 20)

      (HO-nested-letrecs
        "letrec even(odd) = proc(x) if zero?(x) then 1 else (odd -(x,1))
          in letrec odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
               in (odd 13)" 1)

      (begin-test-1
        "begin 1; 2; 3 end"
        3)

      (gensym-test-1 "
         let g = let counter = newref(0) 
                 in proc (dummy) let d = setref(counter, -(deref(counter),-1))
                                 in deref(counter)
         in -((g 11),(g 22))"
         -1)

      (simple-store-test-1 "let x = newref(17) in deref(x)" 17)

      (assignment-test-1 "let x = newref(17) 
                          in begin setref(x,27); deref(x) end" 27)

      (gensym-test-2 "
        let g = let counter = newref(0) 
                in proc (dummy)
                     begin
                      setref(counter, -(deref(counter),-1));
                      deref(counter)
                     end
        in -((g 11),(g 22))"
        -1)

      (even-odd-via-set-1 "
        let x = newref(0)
        in letrec even(d) = if zero?(deref(x)) 
                            then 1
                            else let d = setref(x, -(deref(x),1))
                                 in (odd d)
                  odd(d)  = if zero?(deref(x)) 
                            then 0
                            else let d = setref(x, -(deref(x),1))
                                 in (even d)
           in let d = setref(x,13)
              in (odd -100)"
        1)

      (even-odd-via-set-1 "
        let x = newref(0)
        in letrec even(d) = if zero?(deref(x)) 
                            then 1
                            else let d = setref(x, -(deref(x),1))
                                 in (odd d)
                  odd(d)  = if zero?(deref(x)) 
                            then 0
                            else let d = setref(x, -(deref(x),1))
                                 in (even d)
           in let d = setref(x,13)
              in (odd -100)"
        1)

      (show-allocation-1 "
        let x = newref(22)
        in let f = proc (z)
                    let zz = newref(-(z,deref(x)))
                    in deref(zz)
            in -((f 66), (f 55))"
        11)

      (chains-1 "
        let x = newref(newref(0))
        in begin 
            setref(deref(x), 11);
            deref(deref(x))
          end"
        11)

      (list-1 "[1, 2, 3]" (1 2 3))
      (list-2 "[]" ())
      (cons-1 "cons(42, [])" (42))
      (cons-2 "cons(42, cons(17, []))" (42 17))
      (car-1 "car(cons(42, cons(17, [])))" 42)
      (car-2 "let x = [1, 2, 3] in car(x)" 1)
      (car-3 "car([])" error) ; Make this error more sane, not internal interpreter detail
      (cdr-1 "cdr([])" error) ; Same.
      (cdr-2 "cdr([2,3])" (3))
      (cdr-3 "cdr([3,2,1])" (2 1))
      (null-1 "null?([])" #t)
      (null-2 "null?([1])" #f)
      (null-3 "null?(42)" error) ; Same as above - not needed when type inference finished
      
  )))
