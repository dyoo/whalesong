#lang whalesong

(define (greet name)
  (string-append "hello " name))

(check-expect (greet "danny") "hello danny")
(check-expect (greet "huh") "this should fail")
(check-expect (greet "world") "hello world")
