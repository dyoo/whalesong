#lang s-exp "../../lang/base.rkt"


(printf "Testing with-handlers-2.rkt\n");

(with-handlers ([void (lambda (exn) 'ok)])
  (with-handlers ([1 2])
    (/ 1 0)
    (error "expected an error")))


(with-handlers ([void (lambda (exn) 'ok)])
  (with-handlers ([void 2])
    (/ 1 0)
    (error "expected an error")))

(with-handlers ([void (lambda (exn)
	                (printf "outer\n")
			 (error 'not-ok))])
  (with-handlers ([void (lambda (exn)
    		          'ok)])
    (/ 1 0)
    (error "expected an error")))