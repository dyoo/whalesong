#lang s-exp "../../lang/base.rkt"

(call-with-continuation-prompt 
 (lambda () (printf "Hello world\n")
	 (values 3 4 5))
  (default-continuation-prompt-tag)
  (lambda (thunk)
    (error)))



(call-with-continuation-prompt 
 (lambda (a b c) (printf "~a ~a ~a\n" a b c)
	 (values 3 4 5))
  (default-continuation-prompt-tag)
  (lambda (thunk)
    (error))
  "hello"
  "world"
  "again")



(abort-current-continuation (default-continuation-prompt-tag)
			    (lambda ()
			      (printf "This is the error thunk.")))



(printf "I should not see this\n")