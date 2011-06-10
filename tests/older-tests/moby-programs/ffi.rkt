#lang s-exp "../../lang/base.rkt"


(require "../../ffi/ffi.rkt"
	 "../../lang/check-expect/check-expect.rkt")


(define window
  (js-get-global-value "window"))

(define platform
  (js-get-field window "navigator" "platform"))

(printf "Current browser platform is ~s\n" 
	(prim-js->scheme platform))


(check-expect (prim-js->scheme (scheme->prim-js "hello world"))
	      "hello world")
(check-expect (prim-js->scheme (scheme->prim-js #t))
	      #t)
(check-expect (prim-js->scheme (scheme->prim-js #f))
	      #f)


(printf "minimum and maximum js fixnums are ~a and ~a\n"
	minimum-js-fixnum
	maximum-js-fixnum)



;; (prim-js->scheme (scheme->prim-js ...)) is not the identity, unfortunately.
;; Here are tests that show that we need to do something:

;; Numbers that come back to us are inexact.
(check-expect (inexact->exact (prim-js->scheme (scheme->prim-js 42)))
	      42)

;; Characters are mapped to one-character strings.
(check-expect (prim-js->scheme (scheme->prim-js #\h))
	      "h")

;; Symbols are mapped to strings.
(check-expect (prim-js->scheme (scheme->prim-js 'hello))
	      "hello")

;; Note that when converting vectors, the resulting inner values are not
;; automatically transformed back.  So the prim-js->scheme transformation 
;; is shallow.
(check-expect (map prim-js->scheme 
		   (vector->list (prim-js->scheme (scheme->prim-js #(1 2 3)))))
	      '(1.0 2.0 3.0))

(check-expect (map prim-js->scheme 
		   (vector->list (prim-js->scheme 
				  (scheme->prim-js #(1 "testing" 3)))))
	      '(1.0 "testing" 3.0))






(check-expect (js-=== js-undefined js-undefined) true)
(check-expect (js-=== js-null js-null) true)
(check-expect (js-=== js-undefined js-null) false)
(check-expect (js-=== js-null js-undefined) false)


(check-expect (js-typeof (scheme->prim-js 1)) "number")
(check-expect (js-typeof (scheme->prim-js "hello")) "string")
(check-expect (js-typeof (scheme->prim-js #t)) "boolean")
(check-expect (js-typeof (scheme->prim-js #f)) "boolean")

(check-expect (js-typeof (js-make-hash)) "object")
(check-expect (js-typeof (js-make-hash '(("name" "danny")
					 ("school" "wpi")))) "object")




(define a-hash (js-make-hash '(("foo" "bar")
			       ("baz" "blah"))))
(check-expect (prim-js->scheme (js-get-field a-hash "foo")) "bar")
(check-expect (prim-js->scheme (js-get-field a-hash "baz")) "blah")
(js-set-field! a-hash "something else" (box 3))



;; Uh oh.  There's something about this that I do not understand about
;; the current design of the FFI.  What's going on here?
(check-expect (js-get-field a-hash "something else") 
	      (box 3))



(define my-escape
  (let ([prim-escape (js-get-global-value "escape")])
    (lambda (s)
      (prim-js->scheme (js-call prim-escape #f s)))))
(check-expect (my-escape "hello world") "hello%20world")
(check-expect (my-escape "x.mv,") "x.mv%2C")





(let ([p (procedure->void-js-fun 
	  (lambda () (printf "this is from scheme\n")))])
  (printf "The following should repeat 'this is from scheme'\n")
  (js-call p #f))


(let ([p (procedure->void-js-fun 
	  (lambda (x y) 
	    (printf "I see: ~s ~s\n" 
		    (prim-js->scheme x)
		    (prim-js->scheme y))))])
  (printf "The following should repeat 'I see 3 \"four\"\n")
  (js-call p #f 3 "four"))



(run-tests)
"end of ffi tests"