#lang s-exp "../../lang/wescheme.rkt"


"list tests"

(check-expect (list* 4)
	      4)

(check-expect (list* 1 2 3)
	      (cons 1 (cons 2 3)))

(check-expect (list* 1 2 '(3))
	      (cons 1 (cons 2 (cons 3 empty))))

"end list tests"