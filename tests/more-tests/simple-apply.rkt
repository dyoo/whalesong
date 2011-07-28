#lang planet dyoo/whalesong

(apply + 2 3 4 5 '())
(apply + 2 3 4 '(5))
(apply + 2 3 '(4 5))
(apply + 2 '(3 4 5))
(apply + '(2 3 4 5))


(apply * 2 3 4 5 '())
(apply * 2 3 4 '(5))
(apply * 2 3 '(4 5))
(apply * 2 '(3 4 5))
(apply * '(2 3 4 5))

(apply (lambda args args) 2 3 4 5 '())
(apply (lambda args args) 2 3 4 '(5))
(apply (lambda args args) 2 3 '(4 5))
(apply (lambda args args) 2 '(3 4 5))
(apply (lambda args args) '(2 3 4 5))


(apply (lambda (a b c) (+ a (* b c))) 2 3 '(4))
(add1 (apply (lambda (a b c) (+ a (* b c))) 2 3 '(4)))
(= (apply (lambda (a b c) (+ a (* b c))) 2 3 '(4))
   0)

(apply list 3 4 '(5))


(define (square x)
  (* x x))

(apply square '(7))

"squaring"

(define (square2 x)
  (apply * (list x x)))

(apply square2 '(7))
(apply square2 8 '())


"now factorial"

(define (f x)
  (cond
   [(apply = `(,x 0))
    1]
   [else
    (apply * `(,x ,(apply f (apply sub1 (apply list x '())) '())))]))

(f 0)
;(f 1)
;(f 2)
;(f 3)
;(f 4)
;(f 5)
;(+ (f 4) (f 5))