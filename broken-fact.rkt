#lang planet dyoo/whalesong

(define (f x)
  (if (= x 0)
      "one"
      (* x (f (sub1 x)))))

(f 0)
(list (f 3)
      (f 4))
