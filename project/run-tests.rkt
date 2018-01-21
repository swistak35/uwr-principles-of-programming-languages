#lang eopl

(require rackunit)
(require rackunit/text-ui)
(require (only-in "tests-inference.rkt" inference-tests))
(require (only-in "tests-prettyprinter.rkt" prettyprinter-tests))
(require (only-in "tests-interp.rkt" interp-tests))

(define full-spec
  (make-test-suite
    "All tests"
    (list
      inference-tests
      prettyprinter-tests
      interp-tests)))

(run-tests full-spec 'normal)
