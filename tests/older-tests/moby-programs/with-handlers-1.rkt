#lang s-exp "../../lang/base.rkt"


(printf "with-handlers-1.rkt\n")

(with-handlers ([(lambda (exn)
		   (printf "Is the exception a failure? ~s~n" (exn:fail? exn))
		   (exn:fail? exn))
 	         (lambda (exn)
		   (printf "I'm in the handler and saying ok\n")
		   'ok)])
  (/ 1 0)
  (error 'not-ok))				   



(with-handlers ([(lambda (exn)
		   false)
 	         (lambda (exn)
		   (printf "I'm in the handler and saying ok\n")
		   (error 'not-ok))]
		[(lambda (exn)
		   (printf "second test\n")
		   true)
		 (lambda (exn)
		   'ok)])
  (/ 1 0)
  (error 'not-ok))				   



(with-handlers ([void (lambda (exn) (error 'not-ok))])
  'ok)				   


