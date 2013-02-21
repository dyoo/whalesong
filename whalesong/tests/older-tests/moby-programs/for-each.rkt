#lang s-exp "../../lang/wescheme.rkt"


"for-each"

(for-each (lambda (x) (error 'nothing!)) '())



(define l1 '(hello world this is a test))
(define l2 '(hello this test))
(for-each (lambda (x) (set! l1 (remove x l1)))
	  l2)
(check-expect l1 '(world is a))

"for-each.rkt end"