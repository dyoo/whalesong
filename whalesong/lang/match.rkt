#lang whalesong
(require "private/match/match.rkt" "private/match/runtime.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out "private/match/match.rkt")
                     define-match-expander)
         failure-cont
         (rename-out [define-match-expander* define-match-expander]))


(define-for-syntax (no-old-match-form stx)
  (raise-syntax-error
   #f
   "works only for constructor-based `match' form"
   stx))

(define-syntax-rule (failure-cont) (fail))


(define-syntax define-match-expander*
  (syntax-rules ()
    [(_ id expr) (define-match-expander id expr)]
    [(_ id expr expr2) (define-match-expander id
                         expr
                         no-old-match-form
                         (#%expression expr2))]))

