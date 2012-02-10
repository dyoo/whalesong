#lang planet dyoo/whalesong

(define (mylen x)
  (cond
    [(empty? x)
     0]
    [else
     (add1 (mylen (rest x)))]))
"computing length"
(mylen (build-list 100000 (lambda (i) i)))
"done computing length"