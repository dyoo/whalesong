#lang planet dyoo/whalesong

(define infinite-ones
  (shared ([a (cons 1 a)])
     a))

(car infinite-ones)
(car (cdr infinite-ones))
(car (cdr (cdr infinite-ones)))



(define 1-and-2 (shared ([a (cons 1 b)]
                         [b (cons 2 a)])
                        a))
(car 1-and-2)
(car (cdr 1-and-2))
(car (cdr (cdr 1-and-2)))
(car (cdr (cdr (cdr 1-and-2))))
