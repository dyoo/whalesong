#lang s-exp "../lang/base.rkt"

(provide alert body call-method $
         in-javascript-context?
         viewport-width
         viewport-height

         window
         get-attr
         set-attr!
         js-string?
         string->js-string
         js-string->string
         js-number?
         number->js-number
         js-number->number

         js-null?
         js-null
         
         js-eval
         
         load-script
         
         js-function->procedure
         js-async-function->procedure
         )

(define (alert x)
  (display x)
  (newline))

(define (js-eval x)
  (error 'js-eval "Not available outside JavaScript context"))

(define body 'blah)

(define (call-method object method . args)
  'not-done-yet)

(define ($ name)
  'not-done-yet)

(define window 'not-available-outside-JavaScript-context)


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

(define (js-number? x)
  (error 'js-number? "Not available outside JavaScript context"))
(define (number->js-number x)
  (error 'number->js-number "Not available outside JavaScript context"))
(define (js-number->number x)
  (error 'js-number->number "Not available outside JavaScript context"))



(define (js-null? x)
  (error 'js-null? "Not available outside JavaScript context"))

(define js-null 'not-done-yet)





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



;; load-script: string -> void
;; Load the url as a script.
(define (load-script url)
  (error 'load-script "Not available outside JavaScript context."))


(define (js-function->procedure f)
    (error 'js-function->procedure "Not available outside JavaScript context."))

(define (js-async-function->procedure f)
    (error 'js-async-function->procedure "Not available outside JavaScript context."))