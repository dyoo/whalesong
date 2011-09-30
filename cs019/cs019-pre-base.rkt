#lang s-exp "../lang/base.rkt"


(require (for-syntax "teach.rkt")
         (for-syntax racket/base))

(provide cs019-lambda
         cs019-define
         cs019-when
         cs019-unless
         cs019-set!
         cs019-case)

(define-syntax cs019-define advanced-define/proc)
(define-syntax cs019-lambda advanced-lambda/proc)
(define-syntaxes (cs019-when cs019-unless) (values advanced-when/proc advanced-unless/proc))
(define-syntax cs019-set! advanced-set!/proc)
(define-syntax cs019-case advanced-case/proc)