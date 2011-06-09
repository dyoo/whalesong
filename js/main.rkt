#lang s-exp "../lang/js/js.rkt"

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (alert
                    body
                    call
                    $))