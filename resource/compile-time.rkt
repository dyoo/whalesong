#lang s-exp "../lang/kernel.rkt"
;; Macros for recording the definition of resources in a program.
(require (for-syntax racket/base
                     racket/path
                     racket/port
                     syntax/parse
                     "munge-path.rkt"
                     "record.rkt"))
                     
(provide define-resource)

(require "structs.rkt")


;; file-resource:
;; 
(define-syntax (define-resource stx)
  (syntax-parse stx 
    [(_ name:id)
     (with-syntax ([path (symbol->string (syntax-e #'name))])
       (syntax/loc stx
         (define-resource name path)))]
    [(_ name:id path:str)
     (let* ([normal-path 
             (normalize-path (build-path
                              (or (current-load-relative-directory)
                                  (current-directory))
                              (syntax-e #'path)))]
            [munged-path (munge-path normal-path)]
            [content (call-with-input-file normal-path port->bytes)])
       (with-syntax ([normal-path normal-path]
                     [munged-path munged-path]
                     [content content])
         (syntax/loc stx
           (begin 

             ;; Compile time code:
             (begin-for-syntax
               (let* ([this-module 
                       (variable-reference->resolved-module-path
                        (#%variable-reference))]
                      [resolved-module-path (resolved-module-path-name this-module)])
               (record-resource resolved-module-path normal-path munged-path)))
             
             ;; Run time code
             (define name (resource normal-path munged-path content))))))]))
