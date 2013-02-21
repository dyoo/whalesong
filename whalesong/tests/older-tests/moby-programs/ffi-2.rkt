#lang s-exp "../../lang/base.rkt"
(require "../../ffi/ffi.rkt"
	 "../../jsworld/jsworld.rkt"
	 "../../lang/check-expect/check-expect.rkt")

"my-ffi-2.rkt"

;; Check to see that we can expression on-tick with make-world-config.


(define (my-on-tick world-updater)
  (make-world-config
   (lambda (success)
     (js-call (js-get-global-value "setInterval")
	      #f
	      (procedure->void-js-fun (lambda args (js-call success #f)))
	      1000))

   (lambda (id)
     (printf "shutdown with clearInterval id=~s\n" id)
     (js-call (js-get-global-value "clearInterval")
	      #f
	      id))

   (lambda (w)
     (world-updater w))))




(check-expect (big-bang 1

			(my-on-tick 
			 (lambda (w)
			   (printf "tick!\n")
			   (add1 w)))

			(stop-when 
			 (lambda (n) (= n 10))))
	      10)


(run-tests)
"end my-ffi-2.rkt"