#lang racket/base
(provide f)
(define (f x)
  (* x x))


;; infinite loop
(letrec ([g (lambda () (g))])
  (g))
