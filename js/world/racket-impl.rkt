#lang s-exp "../../lang/base.rkt"

(provide make-js-world-event)

(define (make-world-event-handler setup shutdown)
  (error 'make-world-event-handler "Must be run under a JavaScript context."))
