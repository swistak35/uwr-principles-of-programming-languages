#lang eopl

(require (only-in racket/list
                  remove-duplicates))
(require (only-in racket/base
                  format))
(require (only-in racket/string
                  string-join))
(require "type-data-structures.rkt")
(require "prettyprint-type.rkt")

(provide (all-defined-out))

(define-datatype assumption-set assumption-set?
  (empty-aset)
  (extend-aset 
    (bvar symbol?)
    (btype type-scheme?)
    (saved-aset assumption-set?)))

(define (apply-aset aset var)
  (cases assumption-set aset
    (empty-aset ()
      (eopl:error 'apply-aset "No binding for ~s" var))
    (extend-aset (bvar btype saved-aset)
      (if (eqv? var bvar)
        btype
        (apply-aset saved-aset var)))))

(define (multi-extend-aset b-vars b-types saved-aset)
  (if (null? b-vars)
    saved-aset
    (extend-aset
      (car b-vars)
      (car b-types)
      (multi-extend-aset (cdr b-vars) (cdr b-types) saved-aset))))

(define (aset->list aset)
  (cases assumption-set aset
    (empty-aset () '())
    (extend-aset (b-var b-type saved-aset)
      (cons (list b-var b-type) (aset->list saved-aset)))))

(define (free-var-ids-in-aset aset)
  (remove-duplicates
    (cases assumption-set aset
      (empty-aset () '())
      (extend-aset (bvar btype saved-aset)
        (append (free-var-ids-in-tscheme btype) (free-var-ids-in-aset saved-aset))))))

(define (prettyprint-aset aset)
  (format
    "{~a}"
    (string-join
      (map
        (lambda (aset-elem)
          (format
            "~a: ~a"
            (car aset-elem)
            (prettyprint-tscheme (cadr aset-elem))))
        (aset->list aset))
      "; ")))

