#lang planet dyoo/whalesong
(require (for-syntax racket/base))
(provide x x++ x+1 x=0 get-x)
(define x 0)

(define (set-x v)
  (set! x v))

(define (get-x)
  x)

(define-syntax (x++ stx)
  #'(plusplus-x))

(define-syntax (x+1 stx)
  #'(set-x (add1 x)))

(define (plusplus-x)
  (set! x (add1 x)))

(define-syntax (x=0 stx)
  #'(set-x 0))