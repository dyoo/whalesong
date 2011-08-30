#lang s-exp "../kernel.rkt"

(require (for-syntax racket/base))

(provide traced-app traced-app-key)

(define traced-app-key (gensym 'traced-app-key))


(define-syntax-parameter traced-app
  (lambda (stx)
    (syntax-case stx ()
      [(_ operator operands ...)
       (with-syntax ([key #'traced-app-key]
                     [pos  (vector (format "~s" (syntax-source stx))
                                   (syntax-position stx)
                                   (syntax-line stx)
                                   (syntax-column stx)
                                   (syntax-span stx))])
         (syntax/loc stx
           (with-continuation-mark key 'pos
                                   (#%plain-app operator operands ...))))]
      [else
       stx])))
