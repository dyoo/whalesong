#lang s-exp "../lang/kernel.rkt"
;; Macros for recording the definition of resources in a program.
(require (for-syntax racket/base
                     racket/path
                     syntax/parse
                     "munge-path.rkt"
                     "record.rkt"))
                     
(provide define-resource)

(require "structs.rkt")


;; file-resource:
;; 
(define-syntax (define-resource stx)
  (syntax-parse stx 
    [(_ name:id path:str)
     (let* ([normal-path 
             (normalize-path (build-path
                              (or (current-load-relative-directory)
                                  (current-directory))
                              (syntax-e #'path)))]
            [munged-path (munge-path normal-path)])
       (with-syntax ([normal-path normal-path]
                     [munged-path munged-path])
         (syntax/loc stx
           (begin 
             ;; Compile time code:
             (begin-for-syntax
               (let* ([this-module 
                       (variable-reference->resolved-module-path
                        (#%variable-reference))]
                      [key (resolved-module-path-name this-module)])
                 ;(printf "Recording the resource ~a\n" normal-path)
                 (record-resource munged-path normal-path)))
             
             ;; Run time code
             (define name (resource path munged-path))))))]))
