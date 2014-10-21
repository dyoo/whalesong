#lang whalesong
(provide make-parameter parameterize)

;;;
;;; PARAMETERS
;;;

; This is an implementation of "parameters".
; Consider this a first approximation.

; Parameters in Racket implement a kind of dynamic binding that
; works nicely with threads and continuations.

; Since Whalesong currently is single-threaded there is
; nothing to worry about regarding threads.

; In standard Racket (parameterize bindings body) the bindings
; will be in effect while evaluating body. If control
; leaves body due to exceptions, escapes or similar, the
; bindings are reverted to their previous values. On re-entry
; to body (e.g. from a captured continuation) the bindings
; are supposed to be reinstated.

; Given dynamic-wind this behaviour is straight-forward to implement.
; Alas Whalesong does not support dynamic-wind yet, so in this
; implementation nothing happens, when control leaves body.

; In short: For programs that don't use call/cc and friends
; this implementation of parameters ought to work as expected.

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

; Parameterization can be nested, so we represent the parameter cell
; as a stack of values. 
(struct parameter (values) #:mutable)

; Each parameter will get an unique id.
(define *parameters* '())   ; Assocation list from ids to parameter cells.

; syntax : (push! id-expr)
;  push a new parameter cell to *parameters*
(define-syntax (push! stx)
  (syntax-case stx ()
    [(_ val)
     #'(set! *parameters* (cons val *parameters*))]))

; find-parameter : id -> parameter
;   return parameter associated with id
(define (find-parameter id)
  (cond
    [(assq id *parameters*) => cdr]
    [else (error 'find-parameter "parameter not found, got id: ~a" id)]))

; make-parameter : value -> parameter-procecure
;   Make new parameter and return its parameter procedure.
;   The parameter procedure also acts as id for the parameter.
(define (make-parameter val)
  (define p (parameter (list val)))
  (define proc (case-lambda
                 [()  (first (parameter-values (find-parameter proc)))]
                 [(v) (define p (find-parameter proc))
                      (define vs (cons v (parameter-values p)))
                      (set-parameter-values! p vs)]))
  (push! (cons proc p))
  proc)

; syntax : (parameterize bindings body ...)
;   Evaluate body where the parameters in bindings
;   are bound to the values given in bindings.
;   Restore bindings afterwards.
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
