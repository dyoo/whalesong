#lang racket/base

(provide big-bang initial-view stop-when)

(define (big-bang world . handlers)
  (error 'big-bang "Please run in JavaScript context."))

(define (initial-view a-view-or-resource)
  (error 'initial-view "Please run in JavaScript context."))

(define (stop-when a-view-or-resource)
  (error 'stop-when "Please run in JavaScript context."))