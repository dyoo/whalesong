#lang s-exp "../../lang/wescheme.rkt"

(check-expect (identity 42) 42)

(define p (cons 3 4))
(check-expect (identity p) p)