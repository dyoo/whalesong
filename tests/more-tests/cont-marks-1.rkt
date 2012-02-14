#lang planet dyoo/whalesong/base

(define (puzzle n)
  (if (= n 0)
      (continuation-mark-set->list (current-continuation-marks) 'secret)
      (with-continuation-mark 'secret
        (* n (first (continuation-mark-set->list (current-continuation-marks) 'secret)))
        (puzzle (sub1 n)))))

(continuation-mark-set->list (current-continuation-marks) 'secret)

(with-continuation-mark 'secret 1
  (puzzle 3))
