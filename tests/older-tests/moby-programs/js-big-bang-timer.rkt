#lang s-exp "../../lang/wescheme.rkt"

(require "../../jsworld/jsworld.rkt")


(printf "js-big-bang-timer.rkt\n")
(printf "number should be counting up to ten\n")
(check-expect (big-bang 1 
			   (on-tick (lambda (w)
				      (printf "~s~n" w)
				      (add1 w))
				    1/4)
			   (stop-when (lambda (w) (= w 10))))
	      10)
