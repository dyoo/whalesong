#lang planet dyoo/whalesong

(define (mylen x acc)
  (cond
    [(empty? x)
     acc]
    [else
     (mylen (rest x) (add1 acc))]))
"computing length"
(define v (build-list 1000000 (lambda (i) i)))
(printf "Built list\n")
(mylen v 0)
"done computing length"