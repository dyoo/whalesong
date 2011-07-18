#lang s-exp "../lang/js/js.rkt"

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript (
               ;; the raw implementation doesn't know anything about
               ;; Whalesong.
               "private/raw-jsworld.js"  

               ;; We add Whalesong-specific things here.
               "kernel.js"
               "js-impl.js"
               )
 #:provided-values (big-bang
                    on-tick
                    to-draw
                    stop-when))