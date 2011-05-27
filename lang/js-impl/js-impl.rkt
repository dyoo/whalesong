#lang racket/base

;; Special language level where implementation is done in Javascript.

(require (for-syntax racket/base)
         (for-syntax racket/file)
         (for-syntax syntax/modresolve)
         (for-syntax "record.rkt"))


(define-for-syntax (read-implementation a-module-path)
  (let ([a-path (parameterize ([current-directory (or (current-load-relative-directory)
                                                      (current-directory))])
                  (resolve-module-path a-module-path #f))])
    (file->string a-path)))
    

(define-syntax (require-js stx)
  (syntax-case stx ()
    [(_ path ...)
     (andmap (compose string? syntax-e) (syntax->list #'(path ...)))
     (with-syntax 
         ([(impl ...) (map (compose read-implementation syntax-e)
                           (syntax->list #'(path ...)))])
       (syntax/loc stx
         (begin
           (begin-for-syntax
             (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-implementations! key (list (#%datum . impl) ...))))
           (void))))]))

                                       
(define-syntax (-provide stx)
  (syntax-case stx ()
    [(_ name ...)
     (andmap (compose symbol? syntax-e) (syntax->list #'(name ...)))
     (syntax/loc stx
       (begin
         (begin-for-syntax
           (let* ([this-module (variable-reference->resolved-module-path (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-exports! key (list (#%datum . name) ...))))
         (provide name ...)
         (begin (define name (lambda args 
                               (error (quote name) 
                                      "Must be evaluated within Javascript"))) ...)))]))




           
(provide require-js
         require
	 planet
	 (rename-out (-provide provide)
                     (#%plain-module-begin #%module-begin)))
