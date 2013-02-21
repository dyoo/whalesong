#lang s-exp "../../lang/wescheme.rkt"

(check-error (/ 1 0) "/: division by zero")


#;(define-struct foo ())
#;(check-error (make-foo 3 4)
             "make-foo: expects no arguments, given 2: 3 4")


#;(define (f x)
  (* x x))
#;(check-error (f 3 4) "procedure f: expects 1 argument, given 2: 3 4")