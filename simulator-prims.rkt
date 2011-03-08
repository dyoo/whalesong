#lang racket/base
(require "simulator-structs.rkt"
         racket/math
         (for-syntax racket/base))


(define-syntax (make-lookup stx)
  (syntax-case stx ()
    [(_ #:functions (name ...)
        #:constants (cname ...))
     (with-syntax ([(prim-name ...) (generate-temporaries #'(name ...))])
       (syntax/loc stx
         (let ([prim-name (make-primitive-proc 
                           (lambda args
                             (apply name args)))]
               ...)
           (lambda (n)
             (cond
               [(eq? n 'name)
                prim-name]
               ...
               [(eq? n 'cname)
                cname]
               ...
               [else
                (make-undefined)]
               )))))]))


(define e (exp 1))

(provide lookup-primitive)
(define lookup-primitive (make-lookup #:functions (+ - * / = < <= > >= cons list car cdr
                                                     sub1
                                                     display newline displayln)
                                      #:constants (null pi e)))
