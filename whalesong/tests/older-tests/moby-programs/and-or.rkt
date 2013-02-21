#lang s-exp "../../lang/wescheme.ss"

(printf "and-or.rkt\n")

(check-expect (and true "hello") "hello")
(check-expect (or #f #f "world" 'dontcomehere)
	      "world")


(check-expect (not 3) false)
(check-expect (not (not 3)) true)

(printf "and-or.rkt end\n")