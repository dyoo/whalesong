#lang scheme/base
(require (for-syntax racket/base 
                     racket/file
                     racket/string
                     syntax/parse
                     syntax/modresolve
                     "record.rkt"))

(define-for-syntax (my-resolve-path a-module-path)
  (parameterize ([current-directory (or (current-load-relative-directory)
                                        (current-directory))])
    (resolve-module-path a-module-path #f)))
  

(define-for-syntax (resolve-implementation-path a-module-path)
  (let ([a-path (my-resolve-path a-module-path)])
    (path->string a-path)))


(define-syntax (declare-implementation stx)
  (syntax-parse stx
    [(_ #:racket racket-module-name
        #:javascript (javascript-module-name ...)
        #:provided-values (provided-name ...))
     (with-syntax 
         ([resolved-racket-module-name 
           (my-resolve-path (syntax-e #'racket-module-name))]
          [impl
           (map (compose resolve-implementation-path syntax-e)
                (syntax->list #'(javascript-module-name ...)))]
          [(internal-name ...) (generate-temporaries #'(provided-name ...))]
          [(set-internal-name! ...) (generate-temporaries #'(provided-name ...))])
       (syntax/loc stx
         (begin
           
           ;; Compile-time code: record the Javascript implementation here.
           ;; Also, record that any references to the racket-module name
           ;; should be redirected to this module.
           (begin-for-syntax
             (let* ([this-module 
                     (variable-reference->resolved-module-path
                      (#%variable-reference))]
                    [key (resolved-module-path-name this-module)])
               (record-redirection! (#%datum . resolved-racket-module-name)
                                    key)
               (record-javascript-implementation! key (#%datum . impl))
               ;;(record-exported-name! key 'internal-name 'provided-name) ...
               ))

           (require racket-module-name)
           (begin
             (define internal-name provided-name)
             ;; Discouraging constant folding via set! to address issue 74
             ;; https://github.com/dyoo/whalesong/issues/74
             (define (set-internal-name! x)
               (set! internal-name x)))
           ...
           (provide (rename-out [internal-name provided-name] ...)))))]))


(define-syntax (my-require stx)
  (syntax-case stx ()
    [(_ module-path ...)
     (andmap (lambda (p) (module-path? (syntax-e p)))
             (syntax->list #'(module-path ...)))
     (with-syntax ([(required-path ...)
                    (map (lambda (p)
                           (my-resolve-path (syntax-e p)))
                         (syntax->list #'(module-path ...)))])
       (syntax/loc stx
         (begin
           (begin-for-syntax
            (let* ([this-module 
                    (variable-reference->resolved-module-path
                     (#%variable-reference))]
                   [key (resolved-module-path-name this-module)])
              (record-module-require! key 'required-path)
              ...
              (void)))
           (void))))]
    [else
     (raise-syntax-error #f "Expected module path" stx)]))
     


(provide declare-implementation
         (rename-out [#%plain-module-begin #%module-begin]
                     [my-require require]))