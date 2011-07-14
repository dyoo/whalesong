#lang s-exp "../lang/base.rkt"

(provide big-bang
         on-tick)


(define (big-bang initial-world . args)
  (error 'big-bang "not done yet"))

(define on-tick
  (case-lambda [(handler)
                (error 'on-tick "not done yet")]
               [(handler interval)
                (error 'on-tick "not done yet")]))