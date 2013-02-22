#lang whalesong
;; This should invoke the use of inline-variant from 5.2.1
(provide f)
(define (f x) x)
(f 2)
