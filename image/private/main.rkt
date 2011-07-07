#lang s-exp "../../lang/js/js.rkt"

;; We need to make sure the color module has been invoked
;; before invoking this module, since the JavaScript implementation
;; depends on it.
(require "color.rkt")

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("colordb.js"
               "kernel.js"
               "js-impl.js")
 #:provided-values (is-color?))


