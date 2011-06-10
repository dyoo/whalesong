#lang s-exp "../../lang/wescheme.rkt"

(printf "42.rkt\n")

(define (f x)
  (* x x))

(check-expect (format "~s ~s ~s\n" 
	              (f 16)
        	      (f -5)
        	      (f 42))
	      "256 25 1764\n")
