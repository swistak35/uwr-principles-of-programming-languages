(module top (lib "eopl.ss" "eopl")

  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require (only-in "drscheme-init.scm"
                    run-tests!))
  ; (require "data-structures.scm")  ; for expval constructors
  ; (require "lang.scm")             ; for scan&parse
  ; (require "interp.scm")           ; for value-of-program
  (require (only-in "prettyprinter.scm"
                    pretty-print))
  (require (only-in "tests-prettyprinter.scm"
                    test-list))

  (define (run-all)
    (run-tests! pretty-print equal? test-list))

  (run-all)

  )
