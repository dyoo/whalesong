#lang racket/base

(require "simulator-structs.rkt")
(provide ensure-primitive-value-box)
(define (ensure-primitive-value-box x)
  (if (and (box? x)
           (PrimitiveValue? (unbox x)))
      x
      (error 'ensure-primitive-value-box "~s" x)))
