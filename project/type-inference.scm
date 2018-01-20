(module prettyprinter (lib "eopl.ss" "eopl")

  ; (require "data-structures.scm")
  ; (require "drscheme-init.scm")
  (require "lang.scm")
  ; (require (only-in racket/base
  ;                   format))
  ; (require (only-in racket/string
  ;                   string-join))
  
  (provide (all-defined-out))

  (define-datatype type type?
    (nat-type)
    (bool-type)
    (list-type
      (elem type?))
    (arrow-type
      (left type?)
      (right type?))
    )

  (define (infer exp)
    (cases expression exp
      (const-exp (num)
        (nat-type))

      ; (var-exp ())
      (else (eopl:error 'infer "Unhandled expression ~s" exp))
      ))

  )
