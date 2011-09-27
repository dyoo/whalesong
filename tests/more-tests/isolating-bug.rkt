#lang planet dyoo/whalesong/base
(require (for-syntax racket/base
                     syntax/struct))


(define-syntax (my-define-struct stx)
  (syntax-case stx ()
    [(_ name (fields ...) kw ...)
     (with-syntax ([(names ...)
                    (build-struct-names #'name
                                        (syntax->list #'(fields ...))
                                        #f
                                        #f)])
       (with-syntax ([cnstr (syntax-case #'(names ...) ()
                              [(struct:name-id constructor misc ...)
                               #'constructor])])
         #'(begin
             (define-values (names ...)
               (let ()
                 (begin
                   (define-struct name (fields ...) kw ...)
                   (let ([cnstr (lambda args
                                  (apply cnstr args))])
                     (values names ...))))))))]))

(my-define-struct swf (f) #:mutable)
make-swf
swf?
swf-f
