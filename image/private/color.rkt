#lang s-exp "../../lang/base.rkt"

(provide [struct-out color])

(define-struct color (red green blue alpha)
  #:extra-constructor-name make-color)


(color 3 4 5 0)
1
(make-color 3 5 7 0)