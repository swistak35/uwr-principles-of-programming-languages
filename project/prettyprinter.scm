(module prettyprinter (lib "eopl.ss" "eopl")

  (require "data-structures.scm")
  (require "drscheme-init.scm")
  (require "lang.scm")
  (require (only-in racket/base
                    format))
  (require (only-in racket/string
                    string-join))

  (provide pretty-print pretty-print/pgm)

  (define (print-indent n)
    (make-string n #\space))

  ; (define (pretty-print exp)
  ;   (pretty-print-aux exp 0))

  (define (pretty-print/pgm pgm)
    (cases program pgm
      (a-program (exp1)
        (pretty-print exp1))))

  (define pretty-print
    (lambda (exp)
      (cases expression exp

        (const-exp (num) (number->string num))

        (diff-exp (exp1 exp2)
          (format "-(~a,~a)" (pretty-print exp1) (pretty-print exp2)))

        (var-exp (var)
          (symbol->string var))

        (if-exp (exp1 exp2 exp3)
          (string-join
            (list
              (format "if ~a" (pretty-print exp1))
              (format "then ~a" (pretty-print exp2))
              (format "else ~a" (pretty-print exp3))
              )
            "\n"))

        (zero?-exp (exp1)
          (format "zero?(~a)" (pretty-print exp1)))

        (let-exp (var exp1 body)
          (string-join
            (list
              (format "let ~a = ~a" (symbol->string var) (pretty-print exp1))
              (format "in ~a" (pretty-print body)))
            "\n"))

        (proc-exp (var body)
          (format "proc(~a) ~a" (symbol->string var) (pretty-print body)))

        (call-exp (rator rand)
          (format
            "(~a ~a)"
            (pretty-print rator)
            (pretty-print rand)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (string-join
            (list
              (format "letrec ~a(~a) = ~a"
                (symbol->string (car p-names))
                (symbol->string (car b-vars))
                (pretty-print (car p-bodies)))
              (string-join
                (map
                  (lambda (p-name b-var p-body)
                    (format "       ~a(~a) = ~a" p-name b-var (pretty-print p-body)))
                  (cdr p-names) (cdr b-vars) (cdr p-bodies))
                "\n")
              (format "in ~a" (pretty-print letrec-body)))
            "\n"))

        (begin-exp (exp1 exps)
          (string-join
            (list
              "begin"
              (string-join
                (map
                  (lambda (expn)
                    (format "  ~a" (pretty-print expn))) (cons exp1 exps))
                ";\n")
              "end")
            "\n"))

        (newref-exp (exp1)
          (format "newref(~a)" (pretty-print exp1)))

        (deref-exp (exp1)
          (format "deref(~a)" (pretty-print exp1)))

        (setref-exp (exp1 exp2)
          (format "setref(~a, ~a)" (pretty-print exp1) (pretty-print exp2)))

        (list-exp (exps)
          (format
            "[~a]"
            (string-join
              (map pretty-print exps)
              ", ")
            ))

        (car-exp (exp1)
          (format "car(~a)" (pretty-print exp1)))

        (cdr-exp (exp1)
          (format "cdr(~a)" (pretty-print exp1)))

        (null?-exp (exp1)
          (format "null?(~a)" (pretty-print exp1)))

        (cons-exp (exp1 exp2)
          (format "cons(~a, ~a)" (pretty-print exp1) (pretty-print exp2)))

        )))
  )
