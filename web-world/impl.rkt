#lang s-exp "../lang/js/js.rkt"


;; Make sure the resource library is loaded.
(require "../resource.rkt")


(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (big-bang
                    initial-view
                    stop-when))
