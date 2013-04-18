#lang s-exp "kernel.rkt"

;; Acts like the "Pretty Big" kind of language; has several features turned on by default.
;; These include:
;;
;;     * Automatically running tests
;;     * Annotating all applications so they produce stack traces on error
;;     * Adding the "shared" form by default.
;;     * define-struct automatically has #:transparent and #:mutable


(require "base.rkt"
         "private/traced-app.rkt"
         "private/shared.rkt"
         "private/call-ec.rkt"
         "check-expect/check-expect.rkt"
         "bool.rkt"
         "posn.rkt"
         (for-syntax racket/base))

;; Programs written in Whalesong will have tracing enabled by default.
;; If you don't want this, write in whalesong/base instead.

(provide (except-out (all-from-out "base.rkt")
                     #%app
                     #%module-begin
                     define-struct)
         (rename-out [traced-app #%app]
                     [my-module-begin #%module-begin]
                     [my-define-struct define-struct])
         shared
         (all-from-out "bool.rkt")
         (all-from-out "posn.rkt")
         (except-out (all-from-out "check-expect/check-expect.rkt")
                     run-tests)

         λ)



(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx
       (#%module-begin body ...
                       (run-tests)))]))


(define-syntax λ (make-rename-transformer #'lambda))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...) options ...)
     (let* ([new-options (syntax->list #'(options ...))]
            [new-options (cond [(memq '#:transparent (map syntax-e new-options))
                                new-options]
                           [else
                            (cons (syntax #:transparent) new-options)])]
            [new-options (cond [(memq '#:mutable (map syntax-e new-options))
                                new-options]
                               [else
                                (cons (syntax #:mutable) new-options)])])
       (with-syntax [((new-options ...) new-options)]
         (syntax/loc stx
           (define-struct id (fields ...) new-options ...))))]))


