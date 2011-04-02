#lang racket/base
(require "simulator-structs.rkt"
         racket/math
         (for-syntax racket/base))

(provide lookup-primitive set-primitive!)

(define mutated-primitives (make-hasheq))
(define (set-primitive! n p)
  (hash-set! mutated-primitives n p))


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
                           (lambda (machine . args)
                             (apply name args)))]
               ...)
           (lambda (n)
             (cond
               [(hash-has-key? mutated-primitives n)
                (hash-ref mutated-primitives n)]
               [(eq? n 'exported-name)
                prim-name]
               ...
               [(eq? n 'cname)
                cname]
               ...
               [else
                (make-undefined)]
               )))))]))

;(define call/cc
;  (make-closure call/cc-label
;                1
;                '()
;                'call/cc))
;(define call-with-current-continuation call/cc)

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

(define my-box (lambda (x)
                 (vector x)))

(define my-unbox (lambda (x)
                   (vector-ref x 0)))

(define my-set-box! (lambda (x v)
                      (vector-set! x 0 v)
                      the-void-value))

(define my-vector->list (lambda (v)
                          (apply my-list (vector->list v))))

(define my-list->vector (lambda (l)
                          (apply vector
                          (let loop ([l l])
                            (cond
                              [(null? l)
                               null]
                              [else
                               (cons (MutablePair-h l)
                                     (loop (MutablePair-t l)))])))))
                                

(define my-set-car! (lambda (p v)
                      (set-MutablePair-h! p v)
                      the-void-value))

(define my-set-cdr! (lambda (p v)
                      (set-MutablePair-t! p v)
                      the-void-value))

(define my-void (lambda args
                  the-void-value))

(define my-display (lambda args
                     (apply display args)
                     the-void-value))

(define my-displayln (lambda args
                       (apply displayln args)
                       the-void-value))

(define my-newline (lambda args
                     (apply newline args)
                     the-void-value))

(define my-vector-set! (lambda args
                         (apply vector-set! args)
                         the-void-value))


(define lookup-primitive (make-lookup #:functions (+ - * / = < <= > >= 
                                                     sub1
                                                     not
                                                     null?
                                                     eq?
                                                     add1
                                                     sub1
                                                     zero?
                                                     abs
                                                     (my-void void)
                                                     quotient
                                                     remainder
 
                                                     (my-display display)
                                                     (my-displayln displayln)
                                                     (my-newline newline)
                                                    
                                                     symbol->string
                                                     string-append
                                                     string-length
                                                     
                                                     (my-cons cons)
                                                     (my-list list)
                                                     (my-car car)
                                                     (my-cdr cdr)
                                                     (my-pair? pair?)
                                                     (my-set-car! set-car!)
                                                     (my-set-cdr! set-cdr!)
                                                     
                                                     
                                                     
                                                     (my-box box)
                                                     (my-unbox unbox)
                                                     (my-set-box! set-box!)
                                                     
                                                     vector
                                                     (my-vector-set! vector-set!)
                                                     vector-ref
                                                     (my-vector->list vector->list)
                                                     (my-list->vector list->vector)
                                                     
                                                     
                                                     
                                                     equal?
                                                     
                                                     
                                                    
                                                     
                                                     symbol?)
                                      #:constants (null pi e 
                                                        #;call/cc
                                                        #;call-with-current-continuation)))


