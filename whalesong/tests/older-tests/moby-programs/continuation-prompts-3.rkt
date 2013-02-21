#lang s-exp "../../lang/base.rkt"


(define n 0)
(define (f)
  (call-with-continuation-prompt
   (lambda ()
     (abort-current-continuation (default-continuation-prompt-tag)
                                 (lambda ()
                                   (set! n (add1 n))
                                   (when (< n 10000)
                                     (f)))))
   (default-continuation-prompt-tag)
   (lambda (thunk)
     (thunk))))

(f)

n