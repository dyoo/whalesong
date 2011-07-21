#lang s-exp "../lang/base.rkt"

(provide big-bang
         on-tick
         to-draw
         on-key
         key=?
         stop-when)


;; Fixme: the errors below need to be replaced with 2htdp/world-based
;; implementations.


(define (big-bang initial-world . args)
  (error 'big-bang "not done yet"))


(define on-tick
  (case-lambda [(handler)
                (error 'on-tick "not done yet")]
               [(handler interval)
                (error 'on-tick "not done yet")]))

(define (to-draw handler)
  (error 'to-draw "not done yet"))

(define (on-key handler)
  (error 'on-key "not done yet"))

(define (key=? key-1 key-2)
  (error 'key=? "not done yet"))

(define (stop-when handler)
  (error 'stop-when "not done yet"))
