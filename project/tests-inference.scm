(module tests mzscheme

  (require "lang.scm")
  (require "type-inference.scm")

  (provide test-list)

  (define test-list
    `(
      (positive-const
        ,(const-exp 11)
        ,(nat-type))

      (iszero-1
        ,(zero?-exp (const-exp 42))
        ,(bool-type))

      (iszero-2
        ,(zero?-exp (zero?-exp (const-exp 42)))
        error)

      )
    )
  )
