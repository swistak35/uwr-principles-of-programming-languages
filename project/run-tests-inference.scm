(module run-tests-inference (lib "eopl.ss" "eopl")

  (require (only-in "drscheme-init.scm"
                    run-tests!))
  (require (only-in "type-inference.scm"
                    infer))
  (require (only-in "tests-inference.scm"
                    test-list))

  (define (run-all)
    (run-tests! infer equal? test-list))

  (run-all)

  )
