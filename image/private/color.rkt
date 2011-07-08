#lang s-exp "../../lang/base.rkt"

(provide (except-out [struct-out color] color make-color)
         [rename-out [-color make-color]
                     [-color color]])

(define-struct color (red green blue alpha)
  #:extra-constructor-name make-color)

(define -color
  (case-lambda
    [(r g b)
     (color r g b 255)]
    [(r g b a)
     (color r g b a)]))