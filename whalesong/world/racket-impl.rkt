#lang s-exp "../lang/base.rkt"

(provide big-bang
         to-draw
         on-tick
         on-mouse
         on-key
         on-release
         key=?
         stop-when)


;; Fixme: the errors below need to be replaced with 2htdp/world-based
;; implementations.


(define (big-bang initial-world . args)
  (error 'big-bang "must be run in JavaScript context"))


(define on-tick
  (case-lambda [(handler)
                (error 'on-tick "must be run in JavaScript context")]
               [(handler interval)
                (error 'on-tick "must be run in JavaScript context")]))


(define (on-mouse handle)
  (error 'on-mouse "must be run in JavaScript context"))

(define (to-draw handler)
  (error 'to-draw "must be run in JavaScript context"))

(define (on-key handler)
  (error 'on-key "must be run in JavaScript context"))

(define (on-release handler)
  (error 'on-release "must be run in JavaScript context"))

(define (key=? key-1 key-2)
  (error 'key=? "must be run in JavaScript context"))

(define stop-when
  (case-lambda [(handler)
                (error 'stop-when "must be run in JavaScript context")]
               [(handler last-picture)
                (error 'stop-when "must be run in JavaScript context")]))
