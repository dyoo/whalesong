#lang whalesong
; This module contains a poor man's parameters.
(provide make-parameter parameterize)

(require (for-syntax syntax/parse
                     (only-in racket/base ... 
                              with-syntax
                              syntax
                              generate-temporaries
                              #%app)))

; Assumptions:
;   i) single thread
;  ii) no continuation marks available
; The return value of make-parameter is not the parameter structure,
; but the getter/setter. When Whalesong gets support for applicable
; structures, the structure should be returned instead.

(struct param ([value #:mutable] getter guard)
  ; #:property prop:procedure (struct-field-index getter)
  ; Nope - whalesong does not support applicable structures
  )

(define (make-parameter v [guard #f]) ; -> parameter?
  ; return new parameter procedure
  ; the value is initialized to v (in all threads)
  ; setting a new value will pass the value to a guard,
  ; the value returned by the guard will be used as the new value
  ; (the guard can raise an exception)
  ; the guard is not called for the initial value
  (letrec ([getter (λ xs 
                     (if (null? xs)
                         (param-value p)
                         (set-param-value! p (car xs))))]
           [p (param v getter (and guard (λ(x) x)))])
    getter))

(define-syntax (parameterize stx)
  (syntax-parse stx
    [(_ ([param-expr:expr val-expr:expr] ...) body0 body ...)
     (with-syntax ([(param ...)     (generate-temporaries #'(param-expr ...))]
                   [(old-value ...) (generate-temporaries #'(param-expr ...))])
       #'(let ([param param-expr] ...)
           (let ([old-value (param)] ...)
             (param val-expr) ...
             (begin0
               (let () body0 body ...)
               (param old-value) ...))))]))

;;; Tests
#;(begin
    (define foo (make-parameter 11))
    (list (list (foo) (foo 12) (foo))
          (list 11 (void) 12))
    
    (define bar (make-parameter 21))
    
    (list (list (bar) (bar 22) (bar))
          (list 21    (void)   22))
    
    (list (parameterize ([foo 13] [bar 23])
            (list (foo) (bar)))
          (list 13 23))
    
    (list (list (foo) (bar))
          (list 12 22))
    
    (list (parameterize ([foo 13] [bar 23])
            (list (parameterize ([foo 14] [bar 24])
                    (list (foo) (bar)))
                  (foo) (bar)))
          (list (list 14 24) 13 23)))


