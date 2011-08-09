#lang s-exp "../lang/kernel.rkt"

(provide file-resource)


(require "structs.rkt")

;; Macros for recording the definition of resources in a program.
(require (for-syntax racket/base))


;; file-resource:
;; 
(define-for-syntax (file-resource stx)
  (syntax-case stx ()
    [(_ path)
     (syntax/loc stx
       (let-syntax ([compile-time-code
                     (lambda (stx)
                       (displayln "at compile time")
                       #'(void))])
         (resource path)))]))
