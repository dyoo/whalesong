#lang s-exp "../lang/base.rkt"

(require "impl.rkt"
         "helpers.rkt"
         "event.rkt"
         (for-syntax racket/base))

(provide (all-from-out "impl.rkt")
         (all-from-out "helpers.rkt")
         (all-from-out "event.rkt"))

(provide view-bind*)


;; A syntactic form to make it more convenient to focus and bind multiple things
;; (view-bind* a-view
;;             [id type function]
;;             [id type function] ...)
(define-syntax (view-bind* stx)
  (syntax-case stx ()
    [(_ a-view [a-selector a-type a-function] ...)
     (foldl (lambda (a-selector a-type a-function a-view-stx)
              #'(view-bind (view-focus #,a-view-stx #,a-selector)
                           #,a-type
                           #,a-function))
            #'a-view
            (syntax->list #'(a-selector ...))
            (syntax->list #'(a-type ...))
            (syntax->list #'(a-function ...)))]))