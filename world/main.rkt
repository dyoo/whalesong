#lang s-exp "../lang/js/js.rkt"

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("colordb.js"
               "kernel.js"
               "js-impl.js")
 #:provided-values (is-color?))