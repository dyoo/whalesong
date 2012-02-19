#lang planet dyoo/whalesong
(define (fact x)
  (cond
   [(= x 0)
    1]
   [else
    (* x (fact (sub1 x)))]))
(fact 1)
(fact 10)
(fact 100)
(fact 1000)
(fact 10000)
