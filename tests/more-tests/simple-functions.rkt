#lang planet dyoo/whalesong

(provide (all-defined-out))

(define (f x)
  (* x x))

(define (g x)
  (+ x x))

(f 1)
(g 1)
(+ (f 2) (f (g (g 2))))