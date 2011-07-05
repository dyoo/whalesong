#lang planet dyoo/whalesong

;; Knuth's Man-or-boy-test.
;; http://rosettacode.org/wiki/Man_or_boy_test
(define (A k x1 x2 x3 x4 x5)
  (letrec ([B (lambda ()
                (set! k (- k 1))
                (A k B x1 x2 x3 x4))])
    (if (<= k 0)
        (+ (x4) (x5))
        (B))))
(displayln (A 10
              (lambda () 1) 
              (lambda () -1) 
              (lambda () -1)
              (lambda () 1) 
              (lambda () 0)))


