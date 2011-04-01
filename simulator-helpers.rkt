#lang racket/base

(require "simulator-structs.rkt")
(provide ensure-primitive-value-box
         ensure-primitive-value
         ensure-list
         PrimitiveValue->racket
         racket->PrimitiveValue)
(define (ensure-primitive-value-box x)
  (if (and (box? x)
           (PrimitiveValue? (unbox x)))
      x
      (error 'ensure-primitive-value-box "~s" x)))



;; Make sure the value is primitive.
(define (ensure-primitive-value val)
  (let loop ([v val])
    (cond
      [(string? v)
       v]
      [(symbol? v)
       v]
      [(number? v)
       v]
      [(boolean? v)
       v]
      [(null? v)
       v]
      [(VoidValue? v)
       v]
      [(MutablePair? v)
       v]
      [(primitive-proc? v)
       v]
      [(closure? v)
       v]
      [(undefined? v)
       v]
      [(vector? v)
       v]
      [else
       (error 'ensure-primitive-value "~s" v)])))


(define (ensure-list v)
  (cond
    [(null? v)
     v]
    [(and (MutablePair? v)
          (PrimitiveValue? (MutablePair-h v))
          (PrimitiveValue? (MutablePair-t v)))
     v]
    [else
     (error 'ensure-list)]))


(define (PrimitiveValue->racket v)
  (cond
    [(string? v)
     v]
    [(number? v)
     v]
    [(symbol? v)
     v]
    [(boolean? v)
     v]
    [(null? v)
     v]
    [(VoidValue? v)
     (void)]
    [(undefined? v)
     (letrec ([x x]) x)]
    [(primitive-proc? v)
     v]
    [(closure? v)
     v]
    [(vector? v)
     (apply vector (map PrimitiveValue->racket (vector->list v)))]
    [(MutablePair? v)
     (cons (PrimitiveValue->racket (MutablePair-h v))
           (PrimitiveValue->racket (MutablePair-t v)))]))


(define (racket->PrimitiveValue v)
  (cond
    [(string? v)
     v]
    [(number? v)
     v]
    [(symbol? v)
     v]
    [(boolean? v)
     v]
    [(null? v)
     v]
    [(void? v)
     the-void-value]
    [(eq? v (letrec ([x x]) x))
     (make-undefined)]
    [(procedure? v)
     (error 'racket->PrimitiveValue "Can't coerse procedure")]
    [(primitive-proc? v)
     v]
    [(closure? v)
     v]
    [(vector? v)
     (apply vector (map racket->PrimitiveValue (vector->list v)))]
    [(pair? v)
     (make-MutablePair (racket->PrimitiveValue (car v))
                       (racket->PrimitiveValue (cdr v)))]))

