#lang planet dyoo/whalesong/base


(number? 0)
(number? 3.14)
(number? (expt 2 100))
(number? 2+3i)
(number? "42")
(number? 'this-is-a-test)
(number? (list 3 4 5))

(max 3)
(max 3 4 5 6)
(max 3 1 4 1 5 9 2 6)
(max 3 20)
(max 20 3)

(min 3)
(min 3 4 5 6)
(min 3 1 4 1 5 9 2 6)
(min 3 20)
(min 20 3)