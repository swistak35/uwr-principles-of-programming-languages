(module lang (lib "eopl.ss" "eopl")                

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)

      (expression
        ("if" expression "then" expression "else" expression)
        if-exp)

      (expression (identifier) var-exp)

      (expression
        ("let" identifier "=" expression "in" expression)
        let-exp)   

      (expression
        ("proc" "(" identifier ")" expression)
        proc-exp)

      (expression
        ("(" expression (arbno expression) ")")
        call-exp)

      (expression
        ("letrec"
         (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
         "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("newref" "(" expression ")")
        newref-exp)

      (expression
        ("deref" "(" expression ")")
        deref-exp)

      (expression
        ("setref" "(" expression "," expression ")")
        setref-exp)

      (expression
        ("[" (separated-list expression ",") "]")
        list-exp)

      (expression
        ("cons" "(" expression "," expression ")")
        cons-exp)

      (expression
        ("car" "(" expression ")")
        car-exp)

      (expression
        ("cdr" "(" expression ")")
        cdr-exp)

      (expression
        ("null?" "(" expression ")")
        null?-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
