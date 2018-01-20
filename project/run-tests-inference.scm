(module run-tests-inference (lib "eopl.ss" "eopl")

  (require (only-in "drscheme-init.scm"
                    run-tests!))
  (require (only-in "type-inference.scm"
                    infer answer->type))
  (require (only-in "tests-inference.scm"
                    test-list))

  ; (define (equal-answer? ans correct-ans)
  ;   (equal? (answer->type ans) correct-ans))

  (define (run-all)
    (run-tests! infer equal? test-list))

  (run-all)

  )
