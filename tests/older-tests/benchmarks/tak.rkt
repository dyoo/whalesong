#lang s-exp "../../lang/base.rkt"

(provide tak-benchmark)
(require "run-benchmark.rkt")


(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

;;; call: (tak 18 12 6)

(define (tak-benchmark)
  (run-benchmark "Tak" (lambda () (tak 18 12 6)) 10))