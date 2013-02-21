#lang planet dyoo/whalesong/base

(define-struct pair (f r))
(define-struct color (r g b))




(define p1 (make-pair 3 4))
(pair-f p1)
(pair-r p1)



(color-r (make-color 3 4 5))
(color-g (make-color 3 4 5))
(color-b (make-color 3 4 5))