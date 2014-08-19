#lang whalesong
(require (for-syntax racket/base))
(provide make-parameter
         parameterize)

(require (for-syntax syntax/parse))

(struct parameter (values) #:mutable)

(define *parameters* '())
(define *ids* '())

(define-syntax (push! stx)
  (syntax-case stx ()
    [(_ val)
     #'(set! *parameters* (cons val *parameters*))]))

(define (find-parameter id)
  (cond
    [(assq id *parameters*) => cdr]
    [else (error 'find-parameter "parameter not found, got id: ~a" id)]))

(define (make-parameter val)
  (define p (parameter (list val)))
  (define proc (case-lambda
                 [()  (first (parameter-values (find-parameter proc)))]
                 [(v) (define p (find-parameter proc))
                      (define vs (cons v (parameter-values p)))
                      (set-parameter-values! p vs)]))
  (push! (cons proc p))
  proc)

(define-syntax (parameterize stx)
  (syntax-case stx ()
    [(_ ([param-expr val-expr]) body ...)
     #'(let ()
         (define proc param-expr)
         (define p (find-parameter proc))
         (define v  val-expr)
         (define old (parameter-values p))
         (define vs (cons v old))
         (set-parameter-values! p vs)
         (begin0
           body ...
           (set-parameter-values! p old)))]))
