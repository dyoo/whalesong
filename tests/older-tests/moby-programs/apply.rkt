#lang s-exp "../../lang/wescheme.rkt"

(require "../../lang/check-expect/test-expect.rkt")

"apply.rkt"

(check-expect (apply + '()) 0)
(check-expect (apply + '(1 2 3)) 6)
(check-expect (apply + 4 6 '(1 2 3)) 16)

(define f (lambda args args))
(check-expect (apply f 'hello 'world '()) '(hello world))

(let ([g (λ (x) (* x x))])
  (test-expect (apply g 3 '()) 9))


"apply.rkt end"