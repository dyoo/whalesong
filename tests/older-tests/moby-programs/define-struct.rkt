#lang s-exp "../../lang/wescheme.ss"

(printf "define-struct.rkt\n")

(define-struct p (x y))
"should be a structure: " (make-p 3 4)
(check-expect (p? (make-p 3 4))
	      true)
(check-expect (p-x (make-p 3 4)) 3)
(check-expect (p-y (make-p 3 4)) 4)
