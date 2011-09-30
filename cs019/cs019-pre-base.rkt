#lang s-exp "../lang/base.rkt"


(require (for-syntax "teach.rkt")
         (for-syntax racket/base))

(provide cs019-lambda)


(define-syntax cs019-lambda advanced-lambda/proc)
(define-syntaxes (cs019-when cs019-unless) (values advanced-when/proc advanced-unless/proc))