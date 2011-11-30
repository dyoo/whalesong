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


(define (get-attr object attr . other-attrs)
  (error 'get-attr "Not available outside JavaScript context"))


(define (set-attr! obj attr value)
  (error 'set-attr! "Not available outside JavaScript context"))



(define (js-string? x)
  (error 'js-string? "Not available outside JavaScript context"))
(define (string->js-string x)
  (error 'string->js-string "Not available outside JavaScript context"))
(define (js-string->string x)
  (error 'js-string->string "Not available outside JavaScript context"))




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

