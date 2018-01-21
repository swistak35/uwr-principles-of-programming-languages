#lang eopl

(require rackunit)
(require rackunit/text-ui)
(require (only-in "tests-inference.rkt" inference-tests))
(require (only-in "tests-prettyprinter.rkt" prettyprinter-tests))

(define full-spec
  (make-test-suite
    "All tests"
    (list
      inference-tests
      prettyprinter-tests)))

(run-tests full-spec 'normal)
