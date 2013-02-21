#lang s-exp "../lang/base.rkt"

(provide check-not-undefined)

;; Wrapped around uses of local-bound variables:
(define (check-not-undefined name val)
  (if (eq? val undefined)
      (raise
       (make-exn:fail:contract:variable
        (format "local variable used before its definition: ~a" name)
        (current-continuation-marks)
        name))
      val))
(define undefined (letrec ([x x]) x))
