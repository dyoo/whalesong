#lang s-exp "../../lang/wescheme.ss"


(printf "local.rkt\n")

(check-expect (local [(define (f x)
			(* x x))
		      (define (g x)
			(* x x x))]
		     (f (g (g (f 3)))))

	      150094635296999121)