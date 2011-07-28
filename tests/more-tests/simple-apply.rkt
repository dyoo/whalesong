#lang planet dyoo/whalesong

(apply + 2 3 4 5 '())
(apply (lambda args args) 2 3 4 5 '())

"now factorial"

(define (f x)
  (cond
   [(apply = `(,x 0))
    1]
   [else
    (apply * `(,x ,(apply f (apply sub1 (apply list x '())) '())))]))


(f 3)
(+ (f 4) (f 5))