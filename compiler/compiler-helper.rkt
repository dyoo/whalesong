#lang racket/base

(provide ensure-const-value)

(define (ensure-const-value x)
  (cond
   [(symbol? x)
    x]
   [(boolean? x)
    x]
   [(string? x)
    x]
   [(number? x)
    x]
   [(void? x)
    x]
   [(null? x)
    x]
   [(char? x)
    x]
   [(bytes? x)
    x]
   [(path? x)
    x]
   [(pair? x)
    (begin (ensure-const-value (car x))
           (ensure-const-value (cdr x))
           x)]
   [(vector? x)
    (begin (for-each ensure-const-value (vector->list x)))
    x]
   [(box? x)
    (ensure-const-value (unbox x))
    x]
   [else
    (error 'ensure-const-value "Not a const value: ~s\n" x)]))
         
    