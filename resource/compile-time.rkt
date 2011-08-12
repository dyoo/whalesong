#lang s-exp "../lang/kernel.rkt"
;; Macros for recording the definition of resources in a program.
(require (for-syntax racket/base
                     racket/path
                     syntax/parse))
                     
(provide define-resource)

(require "structs.rkt")


;; file-resource:
;; 
(define-syntax (define-resource stx)
  (syntax-parse stx 
    [(_ name:id path:str)
     (with-syntax ([normal-path 
                    (normalize-path (build-path
                                     (or (current-load-relative-directory)
                                         (current-directory))
                                     (syntax-e #'path)))])
       (syntax/loc stx
         (begin (begin-for-syntax
                  (printf "compile time code executing; we need to save ~s\n"
                          normal-path))
                (define name (resource path)))))]))
