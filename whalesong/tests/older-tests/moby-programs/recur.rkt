#lang s-exp "../../lang/wescheme.rkt"


"recur.rkt"
(check-expect
 (recur loop ([i 0])
	(cond [(= i 10) '()]
	      [else
	       (cons (* i i)
		     (loop (add1 i)))]))
 '(0 1 4 9 16 25 36 49 64 81))
       
"recur.rkt end"