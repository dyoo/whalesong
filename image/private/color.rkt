#lang s-exp "../../lang/base.rkt"

(provide [struct-out color])

(define-struct color (red green blue alpha)
  #:extra-constructor-name make-color)
