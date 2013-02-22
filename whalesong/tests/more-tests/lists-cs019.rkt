#lang whalesong/cs019

(define web-colors
 (shared ([W (cons "white" G)]
          [G (cons "grey" W)])
   W))

(check-expect (first web-colors) "white")
(check-expect (second web-colors) "grey")
(check-expect (third web-colors) "white")
(check-expect (fourth web-colors) "grey")
(check-expect (fifth web-colors) "white")
(check-expect (sixth web-colors) "grey")
(check-expect (seventh web-colors) "white")
(check-expect (eighth web-colors) "grey")

(check-expect (equal? (rest web-colors) (rest (rest (rest web-colors)))) true)
(check-expect (eq? (rest web-colors) (rest (rest (rest web-colors)))) true)

