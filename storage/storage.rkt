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
  (unless (exact-nonnegative-integer? i)
    (raise-type-error 'storage-key "natural" i))
  (js-string->string (call-method localStorage "key" (number->js-number i))))

(define (storage-ref name)
  (unless (string? name)
    (raise-type-error 'storage-ref "string" name))
  (define val (call-method localStorage "getItem" (string->js-string name)))
  (if (js-null? val)
      #f
      (js-string->string val)))

(define (storage-set! name value)
  (unless (string? name)
    (raise-type-error 'storage-set! "string" name 0))
  (unless (string? value)
    (raise-type-error 'storage-set! "string" value 1))
  (void (call-method localStorage "setItem"
                     (string->js-string name)
                     (string->js-string value))))

(define (storage-remove! name)
  (unless (string? name)
    (raise-type-error 'storage-remove! "string" name))
  (void (call-method localStorage "removeItem"
                     (string->js-string name))))

(define (storage-clear!)
  (void (call-method localStorage "clear")))