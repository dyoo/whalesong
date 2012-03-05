#lang s-exp "../../lang/base.rkt"

(provide make-js-world-event)

(define (make-js-world-event)
  (error 'make-js-world-event "Must be run under a JavaScript context."))
