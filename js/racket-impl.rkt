#lang s-exp "../lang/base.rkt"

(provide alert body call $)

(define (alert x)
  (display x)
  (newline))

(define body 'blah)

(define (call object method . args)
  'not-done-yet)

(define ($ name)
  'not-done-yet)