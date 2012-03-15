#lang s-exp "../../lang/base.rkt"

(provide make-world-event-handler)

(define (make-world-event-handler setup shutdown)
  (error 'make-world-event-handler "Must be run under a JavaScript context."))
