#lang s-exp "../lang/base.rkt"

(provide big-bang
         on-tick
         to-draw
         stop-when)


;; Fixme: replace with 2htdp/world stuff


(define (big-bang initial-world . args)
  (error 'big-bang "not done yet"))


(define on-tick
  (case-lambda [(handler)
                (error 'on-tick "not done yet")]
               [(handler interval)
                (error 'on-tick "not done yet")]))

(define (to-draw handler)
  (error 'on-tick "not done yet"))


(define (stop-when handler)
  (error 'on-tick "not done yet"))
