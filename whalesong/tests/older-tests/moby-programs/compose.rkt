#lang s-exp "../../lang/wescheme.rkt"

"compose.rkt"

(define (f x) (* x x))
(define (g x) (+ x x))

(check-expect (procedure? (compose f g)) true)
(check-expect ((compose f g) 7)
	      (* 14 14))

(check-expect ((compose) 7)
	      7)

(check-expect ((compose f) 7)
	      49)

"compose.rkt end"