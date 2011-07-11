#lang planet dyoo/whalesong
(provide fact)
(define (fact x)
  (cond
   [(= x 0)
    1]
   [else
    (* x (fact (sub1 x)))]))


(printf "test: ~s\n" (fact 4))