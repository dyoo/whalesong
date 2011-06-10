#lang s-exp "../../lang/wescheme.rkt"

"exn.rkt"


(define e0 (make-exn "blah" (current-continuation-marks)))
(define e1 (make-exn:fail "foo" (current-continuation-marks)))
(define e2 (make-exn:fail:contract "bar" (current-continuation-marks)))
(define e3 (make-exn:fail:contract:arity "baz" (current-continuation-marks)))
(define e4 (make-exn:fail:contract:divide-by-zero "blip" (current-continuation-marks)))
(define e5 (make-exn:fail:contract:variable "blop" (current-continuation-marks) 'x))


(check-expect (exn? e0) true)
(check-expect (exn? e1) true)
(check-expect (exn? e2) true)
(check-expect (exn? e3) true)
(check-expect (exn? e4) true)
(check-expect (exn? e5) true)

(check-expect (exn-message e0) "blah")
(check-expect (exn-message e1) "foo")
(check-expect (exn-message e2) "bar")
(check-expect (exn-message e3) "baz")
(check-expect (exn-message e4) "blip")
(check-expect (exn-message e5) "blop")

(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e0)) true)
(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e1)) true)
(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e2)) true)
(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e3)) true)
(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e4)) true)
(check-expect (continuation-mark-set? 
	        (exn-continuation-marks e5)) true)


(check-expect (exn:fail? e0) false)
(check-expect (exn:fail? e1) true)
(check-expect (exn:fail? e2) true)
(check-expect (exn:fail? e3) true)
(check-expect (exn:fail? e4) true)
(check-expect (exn:fail? e5) true)

(check-expect (exn:fail:contract? e0) false)
(check-expect (exn:fail:contract? e1) false)
(check-expect (exn:fail:contract? e2) true)
(check-expect (exn:fail:contract? e3) true)
(check-expect (exn:fail:contract? e4) true)
(check-expect (exn:fail:contract? e5) true)

(check-expect (exn:fail:contract:arity? e0) false)
(check-expect (exn:fail:contract:arity? e1) false)
(check-expect (exn:fail:contract:arity? e2) false)
(check-expect (exn:fail:contract:arity? e3) true)
(check-expect (exn:fail:contract:arity? e4) false)
(check-expect (exn:fail:contract:arity? e5) false)

(check-expect (exn:fail:contract:variable? e0) false)
(check-expect (exn:fail:contract:variable? e1) false)
(check-expect (exn:fail:contract:variable? e2) false)
(check-expect (exn:fail:contract:variable? e3) false)
(check-expect (exn:fail:contract:variable? e4) false)
(check-expect (exn:fail:contract:variable? e5) true)


(check-expect (exn:fail:contract:divide-by-zero? e0) false)
(check-expect (exn:fail:contract:divide-by-zero? e1) false)
(check-expect (exn:fail:contract:divide-by-zero? e2) false)
(check-expect (exn:fail:contract:divide-by-zero? e3) false)
(check-expect (exn:fail:contract:divide-by-zero? e4) true)
(check-expect (exn:fail:contract:divide-by-zero? e5) false)


"exn.rkt end"