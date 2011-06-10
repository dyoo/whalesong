#lang s-exp "../../lang/wescheme.rkt"
(require "../../lang/check-expect/test-expect.rkt")

"letrec"

(letrec ([even? (lambda (x)
		  (if (= x 0)
		      true
		      (odd? (sub1 x))))]
	 [odd? (lambda (x)
		 (if (= x 0)
		     false
		     (even? (sub1 x))))])
  (test-expect (even? 1024) true)
  (test-expect (even? 1023) false)
  (test-expect (even? 2172) true)
  (test-expect (even? 2171) false))




(letrec-values ([(even? odd?)
		 (values
		  (lambda (x)
		    (if (= x 0)
			true
			(odd? (sub1 x))))
		  (lambda (x)
			 (if (= x 0)
			     false
			     (even? (sub1 x)))))])
  (test-expect (even? 1024) true)
  (test-expect (even? 1023) false)
  (test-expect (even? 2172) true)
  (test-expect (even? 2171) false))





(letrec ([fact (lambda (x)
		 (if (= x 0)
		     1
		     (* x (fact (sub1 x)))))])
  (test-expect (fact 3) 6)
  (test-expect (fact 4) 24)
  (test-expect (fact 5) 120))


"letrec.rkt end"
