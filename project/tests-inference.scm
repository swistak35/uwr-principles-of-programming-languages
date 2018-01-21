(module tests mzscheme

  (require "lang.scm")
  (require "type-inference.scm")

  (provide test-list)

  (define test-list
    `(
      (positive-const
        ,(const-exp 11)
        ,(int-type))

      (iszero-1
        ,(zero?-exp (const-exp 42))
        ,(bool-type))

      (iszero-2
        ,(zero?-exp (zero?-exp (const-exp 42)))
        error)

      (if-1
        ,(if-exp (zero?-exp (const-exp 42)) (const-exp 42) (const-exp 42))
        ,(int-type))

      (if-2-cond-must-be-bool
        ,(if-exp (const-exp 42) (const-exp 42) (const-exp 42))
        error)

      (if-2-results-must-be-the-same
        ,(if-exp (zero?-exp (const-exp 42)) (zero?-exp (const-exp 42)) (const-exp 42))
        error)


      )
    )
  )
