#lang racket/base
(require "simulator-structs.rkt"
         "simulator-helpers.rkt"
         "../compiler/il-structs.rkt"
         racket/math
         racket/list
         (for-syntax racket/base))

(provide lookup-primitive set-primitive!)

(define mutated-primitives (make-hasheq))
(define (set-primitive! n p)
  (hash-set! mutated-primitives n p))


(define (extract-arity proc)
  (let loop ([racket-arity (procedure-arity proc)])
    (cond
      [(number? racket-arity)
       racket-arity]
      [(arity-at-least? racket-arity)
       (make-ArityAtLeast (arity-at-least-value racket-arity))]
      [(list? racket-arity)
       (map loop racket-arity)])))
                



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
                             (apply name args))
                           (extract-arity name)
                           'exported-name)]
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


(define my-cadr (lambda (x)
                  (MutablePair-h (MutablePair-t x))))

(define my-caddr (lambda (x)
                  (MutablePair-h (MutablePair-t (MutablePair-t x)))))


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



(define my-member (lambda (x l)
                    (let loop ([l l])
                      (cond
                        [(null? l)
                         #f]
                        [(MutablePair? l)
                         (cond
                           [(equal? x (MutablePair-h l))
                            l]
                           [else
                            (loop (MutablePair-t l))])]
                        [else
                         (error 'member "not a list: ~s" l)]))))

(define my-reverse (lambda (l)
                    (let loop ([l l]
                               [acc null])
                      (cond
                        [(null? l)
                         acc]
                        [(MutablePair? l)
                         (loop (MutablePair-t l)
                               (make-MutablePair (MutablePair-h l) acc))]
                        [else
                         (error 'member "not a list: ~s" l)]))))


(define my-printf (lambda (fmt args)
                     (apply printf fmt (map (lambda (x)
                                              (PrimitiveValue->racket x))
                                            args))))



(define current-continuation-marks   
  (letrec ([f (case-lambda [(a-machine)
                            (f a-machine default-continuation-prompt-tag-value)]
                           [(a-machine tag)
                            (make-ContinuationMarkSet
                             (let loop ([frames (machine-control a-machine)])
                               (cond
                                 [(empty? frames)
                                  empty]
                                 [else
                                  (append (hash-map (frame-marks (first frames))
                                                    cons)
                                          (if (eq? tag (frame-tag (first frames)))
                                              empty
                                              (loop (rest frames))))])))])])
    (make-primitive-proc (lambda (machine . args) (apply f machine args))
                         '(0 1)
                         'current-continuation-marks)))
                        

(define continuation-mark-set->list
  ;; not quite correct: ContinuationMarkSets need to preserve frame structure a bit more. 
  ;; At the very least, we need to keep track of prompt tags somewhere.
  (let ([f (lambda (a-machine mark-set key)
             (let ([marks (ContinuationMarkSet-marks mark-set)])
               (foldr make-MutablePair
                      null
                      (map cdr (filter (lambda (k+v)
                                         (eq? (car k+v) key))
                                       marks)))))])
    (make-primitive-proc (lambda (machine . args) (apply f machine args))
                         '2 ;; fixme: should deal with prompt tags too
                         'current-continuation-marks)))





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
						     (my-cadr cadr)
						     (my-caddr caddr)
                                                     (my-pair? pair?)
                                                     (my-set-car! set-car!)
                                                     (my-set-cdr! set-cdr!)
                                                     (my-member member)
                                                     (my-reverse reverse)
                                                     
                                                     
                                                     (my-box box)
                                                     (my-unbox unbox)
                                                     (my-set-box! set-box!)
                                                     
                                                     vector
                                                     (my-vector-set! vector-set!)
                                                     vector-ref
                                                     (my-vector->list vector->list)
                                                     (my-list->vector list->vector)
                                                     vector-length
                                                     make-vector
                                                     
                                                     
                                                     equal?
                                                     symbol?
                                                     
                                                     
                                                     (my-printf printf)
                                                     )
                                      #:constants (null pi e
                                                        current-continuation-marks
                                                        continuation-mark-set->list)))


