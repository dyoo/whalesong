#lang s-exp "../lang/base.rkt"

(provide alert body call-method $
         in-javascript-context?
         viewport-width
         viewport-height)

(define (alert x)
  (display x)
  (newline))

(define body 'blah)

(define (call-method object method . args)
  'not-done-yet)

(define ($ name)
  'not-done-yet)




;; in-javascript-context: -> boolean
;; Produces true if we're in a JavaScript context.
(define (in-javascript-context?)
  #f)


;; viewport-width: -> natural
;; The viewport width in pixels.
(define (viewport-width)
  (error 'viewport-width "Not available outside JavaScript context."))


;; viewport-height: -> natural
;; The viewport height in pixels.
(define (viewport-height)
  (error 'viewport-width "Not available outside JavaScript context."))

