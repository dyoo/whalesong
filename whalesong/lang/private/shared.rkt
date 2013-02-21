#lang s-exp "../kernel.rkt"

(require (for-syntax scheme/base
                     syntax/stx
                     syntax/kerncase
                     syntax/struct
                     racket/struct-info
                     scheme/include)
         "traced-app.rkt")

(provide shared)

(define-for-syntax code-insp (current-code-inspector))

(define undefined (letrec ([x x]) x))
(require (only-in "../kernel.rkt" [cons the-cons]))

(define-syntax shared
  (lambda (stx)
    (define make-check-cdr #f)
    ;; Include the implementation.
    ;; See shared-body.rkt.
    (include "shared-body.rkt")))
