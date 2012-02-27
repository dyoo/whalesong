#lang s-exp "../lang/js/js.rkt"

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (alert
                    body
                    call-method
                    $
                    
                    window
                    
                    get-attr
                    set-attr!

                    js-string?
                    string->js-string
                    js-string->string

                    js-number?
                    number->js-number
                    js-number->number
                    
                    viewport-width
                    viewport-height
                    in-javascript-context?

                    js-null?
                    js-null
                    
                    js-eval
                    ))