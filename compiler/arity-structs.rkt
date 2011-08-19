#lang typed/racket/base

(provide (all-defined-out))

;; Arity
(define-type Arity (U AtomicArity (Listof (U AtomicArity))))
(define-type AtomicArity (U Natural ArityAtLeast))
(define-struct: ArityAtLeast ([value : Natural])
  #:transparent)
(define-predicate AtomicArity? AtomicArity)
(define-predicate listof-atomic-arity? (Listof AtomicArity))

