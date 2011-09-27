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
                 (define-struct name (fields ...) kw ...)
                 (call-with-values (lambda ()
                                     (let ([cnstr (lambda args
                                                    (apply cnstr args))])
                                       (displayln names) ...
                                       (values names ...)))
                                   (lambda args
                                     (displayln "in the result of call-with-values")
                                     (displayln args)
                                     (apply values args))))))))]))







(define-syntax (my-define-struct2 stx)
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
                     (displayln names) ...
                     (values names ...))))))))]))



(my-define-struct swf (f) #:mutable)
(displayln "---")
struct:swf
make-swf
swf?
swf-f
set-swf-f!


(displayln "***")


(my-define-struct swf2 (f) #:mutable)
(displayln "---")
struct:swf2
make-swf2
swf2?
swf2-f
set-swf2-f!
