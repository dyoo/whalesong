#lang s-exp "../lang/base.rkt"


(require (for-syntax "teach.rkt")
         (for-syntax racket/base))

;; FIXME: there's something wrong with the compiler: it's not picking
;; up that teach-runtime is a dependency.
(require "teach-runtime.rkt")

(provide cs019-lambda
         cs019-define
         cs019-when
         cs019-unless
         cs019-set!
         cs019-case
         cs019-local
         cs019-dots)

(define-syntax cs019-define advanced-define/proc)
(define-syntax cs019-lambda advanced-lambda/proc)
(define-syntaxes (cs019-when cs019-unless) (values advanced-when/proc advanced-unless/proc))
(define-syntax cs019-set! advanced-set!/proc)
(define-syntax cs019-case advanced-case/proc)
(define-syntax cs019-local intermediate-local/proc)
(define-syntax cs019-dots beginner-dots/proc)
