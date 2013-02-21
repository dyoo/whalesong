#lang s-exp "../../lang/wescheme.rkt"

"arity.rkt"
(check-expect (procedure-arity (lambda () (void))) 0)
(check-expect (procedure-arity (lambda (x) (void))) 1)
(check-expect (procedure-arity (lambda (x y . z) (void))) 
              (make-arity-at-least 2))

(check-expect (arity-at-least? (make-arity-at-least 0))
              true)

(check-expect (arity-at-least? 'not-an-arity)
              false)

(check-expect (arity-at-least-value
               (make-arity-at-least 7))
              7)

(define f
  (case-lambda [(x y) (list x y)]
               [(x y z) (list x y z)]))
(check-expect (procedure-arity-includes? f 2) true)
(check-expect (procedure-arity-includes? f 3) true)
(check-expect (procedure-arity-includes? f 4) false)
(check-expect (procedure-arity-includes? f 0) false)

(check-expect (procedure-arity-includes? (lambda (x) (* x x)) 1) true)
(check-expect (procedure-arity-includes? (lambda (x) (* x x)) 0) false)
(check-expect (procedure-arity-includes? (lambda args (void)) 0) true)

"arity.rkt end"