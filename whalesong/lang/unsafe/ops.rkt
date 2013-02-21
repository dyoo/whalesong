#lang s-exp "../js/js.rkt"

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (unsafe-car
                    unsafe-cdr))
