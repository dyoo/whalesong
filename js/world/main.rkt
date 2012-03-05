#lang s-exp "../lang/js/js.rkt"
(require "../../web-world.rkt")
(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (make-js-world-event))