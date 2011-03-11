#lang racket/base
(require "simulator-structs.rkt"
         racket/math
         (for-syntax racket/base))

(provide lookup-primitive)


(define-syntax (make-lookup stx)
  (syntax-case stx ()
    [(_ #:functions (name ...)
        #:constants (cname ...))
     (with-syntax ([(prim-name ...) (generate-temporaries #'(name ...))])
       (syntax/loc stx
         (let ([prim-name (make-primitive-proc 
                           (lambda (machine return-label . args)
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

#;(define my-callcc
    (make-primitive-proc
     (lambda (machine return-label k)
       (make-primitive-proc (lambda (m2 r2 k2)
                              ...)))))



(define e (exp 1))

(define lookup-primitive (make-lookup #:functions (+ - * / = < <= > >= cons list car cdr
                                                     sub1
                                                     display newline displayln
                                                     not
                                                     pair?
                                                     eq?
                                                     null?
                                                     add1
                                                     sub1
                                                     abs
                                                     void
                                                     quotient
                                                     remainder
                                                     display
                                                     displayln
                                                     newline)
                                      #:constants (null pi e)))
