#lang whalesong (require "../selfhost-lang.rkt")
(provide (all-defined-out))

;; Arity
(define-type Arity (U AtomicArity (Listof (U AtomicArity))))
(define-type AtomicArity (U Natural ArityAtLeast))
(define-struct ArityAtLeast (value) #:transparent)
; (define-predicate AtomicArity? AtomicArity)
(define (AtomicArity? o) (or (natural? o) (ArityAtLeast? o)))
; (define-predicate listof-atomic-arity? (Listof AtomicArity))
(define (listof-atomic-arity? o)
  (and (list? o) (andmap AtomicArity? o)))

