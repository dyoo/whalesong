#lang racket/base
(require "simulator-structs.rkt"
         "compile.rkt"
         racket/math
         (for-syntax racket/base))

(provide lookup-primitive)


(define-syntax (make-lookup stx)
  (syntax-case stx ()
    [(_ #:functions (name ...)
        #:constants (cname ...))
     (with-syntax ([(prim-name ...) (generate-temporaries #'(name ...))]
                   [((name exported-name) ...)
                    (map (lambda (name)
                           (syntax-case name ()
                             [(real-name exported-name)
                              (list #'real-name #'exported-name)]
                             [_ 
                              (identifier? name)
                              (list name name)]))
                         (syntax->list #'(name ...)))])
       (syntax/loc stx
         (let ([prim-name (make-primitive-proc 
                           (lambda (machine return-label . args)
                             (apply name args)))]
               ...)
           (lambda (n)
             (cond
               [(eq? n 'exported-name)
                prim-name]
               ...
               [(eq? n 'cname)
                cname]
               ...
               [else
                (make-undefined)]
               )))))]))

(define call/cc
  (make-closure call/cc-label
                1
                '()))
(define call-with-current-continuation call/cc)

(define e (exp 1))

(define my-cons (lambda (x y)
                  (make-MutablePair x y)))

(define my-list (lambda args
                   (let loop ([args args])
                     (cond
                       [(null? args)
                        null]
                       [else
                        (make-MutablePair (car args)
                                          (loop (cdr args)))]))))
(define my-car (lambda (x)
                  (MutablePair-h x)))

(define my-cdr (lambda (x)
                  (MutablePair-t x)))

(define my-pair? (lambda (x)
                   (MutablePair? x)))

(define lookup-primitive (make-lookup #:functions (+ - * / = < <= > >= 
                                                     sub1
                                                     display newline displayln
                                                     not
                                                     null?
                                                     eq?
                                                     add1
                                                     sub1
                                                     abs
                                                     void
                                                     quotient
                                                     remainder
                                                     display
                                                     displayln
                                                     newline
                                                     symbol->string
                                                     
                                                     (my-cons cons)
                                                     (my-list list)
                                                     (my-car car)
                                                     (my-cdr cdr)
                                                     (my-pair? pair?)
                                                     vector
                                                     symbol?)
                                      #:constants (null pi e 
                                                        call/cc
                                                        call-with-current-continuation)))


