#lang eopl

(require rackunit)
(require rackunit/text-ui)
(require (only-in "tests-inference.rkt" inference-tests))
(require (only-in "tests-prettyprinter.rkt" prettyprinter-tests))
(require (only-in "tests-unification.rkt" unification-tests))
(require (only-in "tests-interp.rkt" interp-tests))
(require (only-in "tests-type-prettyprinter.rkt" tests-prettyprint-type))

(define full-spec
  (make-test-suite
    "All tests"
    (list
      inference-tests
      prettyprinter-tests
      unification-tests
      interp-tests
      tests-prettyprint-type
      )))

(run-tests full-spec 'normal)
