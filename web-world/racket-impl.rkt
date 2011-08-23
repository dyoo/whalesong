#lang racket/base

(provide big-bang initial-view stop-when on-tick to-draw ->view)

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

(define (to-draw w)
  (error 'to-draw "Please run in JavaScript context."))

(define (->view x)
  (error '->view "Please run in JavaScript context."))