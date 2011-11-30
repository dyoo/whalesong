#lang s-exp "../lang/base.rkt"

;; Bindings to HTML5 storage
;; http://dev.w3.org/html5/webstorage/


(require "../js.rkt")

(provide storage-length
         storage-key
         storage-ref
         storage-set!
         storage-remove!
         storage-clear!)


(define localStorage (get-attr window "localStorage"))

(define (storage-length)
  (inexact->exact (js-number->number (get-attr localStorage "length"))))

(define (storage-key i)
  (js-string->string (call-method localStorage "key" (number->js-number i))))

(define (storage-ref name)
  (js-string->string
   (call-method localStorage "getItem" (string->js-string name))))

(define (storage-set! name value)
  (void (call-method localStorage "setItem"
                     (string->js-string name)
                     (string->js-string value))))

(define (storage-remove! name)
  (void (call-method localStorage "removeItem"
                     (string->js-string name))))

(define (storage-clear!)
  (void (call-method localStorage "clear")))