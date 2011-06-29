#lang s-exp "../lang/base.rkt"

(provide alert body call-method $)

(define (alert x)
  (display x)
  (newline))

(define body 'blah)

(define (call-method object method . args)
  'not-done-yet)

(define ($ name)
  'not-done-yet)
