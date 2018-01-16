(module prettyprinter (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  ; (require "environments.scm")
  ; (require "store.scm")
  (require (only-in racket/base
                    format))
  (require (only-in racket/string
                    string-join))

  (provide pretty-print)

  (define (print-indent n)
    (make-string n #\space))

  ; (define (pretty-print exp)
  ;   (pretty-print-aux exp 0))

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

        (else
          (eopl:error 'pretty-printer "Unsupported ~s" exp)))))

  )
