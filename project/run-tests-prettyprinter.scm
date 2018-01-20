(module top (lib "eopl.ss" "eopl")

  (require (only-in "drscheme-init.scm"
                    run-tests!))
  (require (only-in "prettyprinter.scm"
                    pretty-print))
  (require (only-in "tests-prettyprinter.scm"
                    test-list))

  (define (run-all)
    (run-tests! pretty-print equal? test-list))

  (run-all)

  )
