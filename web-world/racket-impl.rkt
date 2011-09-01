#lang racket/base

(provide big-bang initial-view stop-when
         on-tick
         on-location-change on-mock-location-change
         to-draw

         ->view
         view-focus? view-focus
         view-left view-right view-up view-down
         view-left? view-right? view-up? view-down?
         view-text update-view-text
         view-attr update-view-attr
         view-id

         view-bind

         view-form-value
         update-view-form-value

         view-show
         view-hide
         view-append-child

         open-output-element

         xexp?
         xexp->dom)


(define (big-bang world . handlers)
  (error 'big-bang "Please run in JavaScript context."))

(define (initial-view a-view-or-resource)
  (error 'initial-view "Please run in JavaScript context."))

(define (stop-when f)
  (error 'stop-when "Please run in JavaScript context."))

(define on-tick
  (case-lambda [(f)
                (error 'on-tick "Please run in JavaScript context.")]
               [(f delay)
                (error 'on-tick "Please run in JavaScript context.")]))


(define on-location-change
  (case-lambda [(f)
                (error 'on-location-change "Please run in JavaScript context.")]
               [(f delay)
                (error 'on-location-change "Please run in JavaScript context.")]))


(define on-mock-location-change
  (case-lambda [(f)
                (error 'on-mock-location-change "Please run in JavaScript context.")]
               [(f delay)
                (error 'on-mock-location-change "Please run in JavaScript context.")]))



(define (to-draw w)
  (error 'to-draw "Please run in JavaScript context."))

(define (->view x)
  (error '->view "Please run in JavaScript context."))



(define (view-focus? v selector)
  (error 'view-focus? "Please run in JavaScript context."))

(define (view-focus v selector)
  (error 'view-focus "Please run in JavaScript context."))

(define (view-text v)
  (error 'view-text "Please run in JavaScript context."))



(define (view-left v)
  (error 'view-left "Please run in JavaScript context."))

(define (view-right v)
  (error 'view-right "Please run in JavaScript context."))

(define (view-up v)
  (error 'view-up "Please run in JavaScript context"))

(define (view-down v)
  (error 'view-down "Please run in JavaScript context"))



(define (view-left? v)
  (error 'view-left? "Please run in JavaScript context."))

(define (view-right? v)
  (error 'view-right? "Please run in JavaScript context."))

(define (view-up? v)
  (error 'view-up? "Please run in JavaScript context"))

(define (view-down? v)
  (error 'view-down? "Please run in JavaScript context"))




(define (update-view-text v text)
  (error 'update-view-text "Please run in JavaScript context."))


(define (view-attr v attr-name)
  (error 'view-attr "Please run in JavaScript context."))

(define (update-view-attr v attr-name value)
  (error 'update-view-attr "Please run in JavaScript context."))


(define (view-id v)
  (error 'view-id "Please run in JavaScript context."))


(define (view-bind v type worldF)
  (error 'view-bind "Please run in JavaScript context."))

(define (view-form-value)
  (error 'view-form-value "Please run in JavaScript context."))

(define (update-view-form-value val)
  (error 'view-form-value "Please run in JavaScript context."))


(define (view-show)
  (error 'view-show "Please run in JavaScript context."))

(define (view-hide)
  (error 'view-hide "Please run in JavaScript context."))


(define (view-append-child dom)
  (error 'view-append "Please run in JavaScript context."))


(define (open-output-element id)
  (error 'open-output-element "Please run in JavaScript context."))


(define (xexp? x)
  (error 'xexp? "Please run in JavaScript context."))

(define (xexp->dom x)
  (error 'xexp->dom "Please run in JavaScript context."))
