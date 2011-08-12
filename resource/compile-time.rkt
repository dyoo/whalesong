#lang s-exp "../lang/kernel.rkt"

(provide file-resource)


(require "structs.rkt")

;; Macros for recording the definition of resources in a program.
(require (for-syntax racket/base))


;; file-resource:
;; 
(define-syntax (file-resource stx)
  (syntax-case stx ()
    [(_ path)
     (let ([dontcare
            (syntax-local-lift-expression #'(begin
                                              (begin-for-syntax
                                               (printf "Compile time code executing"))
                                              (void)))])
       (syntax/loc stx
         (let-syntax ([compile-time-code
                       (lambda (stx)
                         (printf "compile time code executing\n")
                         #'(void))])
           (begin
             ;;dontcare
             (resource path)))))]))
