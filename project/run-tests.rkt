#lang eopl

(require rackunit/text-ui)
(require (only-in "tests-inference.rkt" inference-tests))

(run-tests inference-tests)
