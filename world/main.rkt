#lang s-exp "../lang/js/js.rkt"

(require "../image.rkt")

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript (
               ;; the raw implementation doesn't know anything about
               ;; Whalesong.
               "raw-jsworld.js"  

               ;; We add Whalesong-specific things here.
               "kernel.js"
               "js-impl.js"
               )
 #:provided-values (big-bang
                    on-tick
                    on-key
                    key=?
                    to-draw
                    stop-when))



