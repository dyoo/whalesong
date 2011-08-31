#lang s-exp "kernel.rkt"

;; Acts like the "Pretty Big" kind of language; has several features turned on by default.
;; These include:
;;
;;     * Automatically running tests
;;     * Annotating all applications so they produce stack traces on error
;;     * Adding the "shared" form by default.

(require "base.rkt"
         "private/traced-app.rkt"
         "private/shared.rkt"
         "check-expect/check-expect.rkt"
         (for-syntax racket/base))

;; Programs written in Whalesong will have tracing enabled by default.
;; If you don't want this, write in whalesong/base instead.

(provide (except-out (all-from-out "base.rkt")
                     #%app
                     #%module-begin)
         (rename-out [traced-app #%app]
                     [my-module-begin #%module-begin])
         shared
         (except-out (all-from-out "check-expect/check-expect.rkt")
                     run-tests))



(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (#%module-begin body ...
                       (run-tests)))]))