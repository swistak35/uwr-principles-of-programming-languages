(module tests mzscheme

  (require "lang.scm")
  (require "type-inference.scm")

  (provide test-list)

  (define test-list
    `(
      (positive-const
        ,(const-exp 11)
        ,(nat-type))
      )
    )
  )
