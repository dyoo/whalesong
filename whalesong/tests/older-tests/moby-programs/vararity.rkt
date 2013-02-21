#lang s-exp "../../lang/wescheme.rkt"

"vararity"
(define f (lambda args args))

(check-expect (f) empty)
(check-expect (f 1 2) '(1 2))
"vararity done"