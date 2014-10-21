#lang whalesong
(require "parameters.rkt") ; whalesong-libs

(require racket/stxparam
         (for-syntax racket/base))

(provide match-equality-test
         exn:misc:match?
         match:error
         fail
         matchable?
         match-prompt-tag
         mlist? mlist->list)

(define match-prompt-tag (make-continuation-prompt-tag 'match)) 

; (define match-equality-test (make-parameter equal?))
; This is an parameter that a user of match can set in order
; to change the the equality operations used to determine
; if repeated uses of an identifier in a pattern has "equal" values.
; The default is equal?, so in the Whalesong matcher we just hardcode it.
(define match-equality-test (λ () equal?))


(define-struct (exn:misc:match exn:fail) (value srclocs)
 #:property prop:exn:srclocs (lambda (ex) (exn:misc:match-srclocs ex)))


(define (match:error val srclocs form-name)
  (raise (make-exn:misc:match (format "~a: no matching clause for ~e" form-name val)
                              (current-continuation-marks)
                              val
                              srclocs)))

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))

;; can we pass this value to regexp-match?
(define (matchable? e)
  (or (string? e) 
      ; (bytes? e)   ; [Whalesong] no byte strings
      ))

;; duplicated because we can't depend on `compatibility` here
(define mpair? pair?)  ; [Whalesong] no mutable pairs
(define mcdr cdr)      ; [Whalesong]
(define mcar car)
(define (mlist? l)
  (cond
   [(null? l) #t]
   [(mpair? l)
    (let loop ([turtle l][hare (mcdr l)])
      (cond
       [(null? hare) #t]
       [(eq? hare turtle) #f]
       [(mpair? hare)
        (let ([hare (mcdr hare)])
          (cond
           [(null? hare) #t]
           [(eq? hare turtle) #f]
           [(mpair? hare)
            (loop (mcdr turtle) (mcdr hare))]
           [else #f]))]
       [else #f]))]
   [else #f]))

(define (mlist->list l)
  (cond
   [(null? l) null]
   [else (cons (mcar l) (mlist->list (mcdr l)))]))
