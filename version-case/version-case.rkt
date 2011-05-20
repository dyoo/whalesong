#lang racket/base

(require (for-syntax racket/base)
         (for-syntax racket/bool)
         (for-syntax "version-misc.rkt"))


(provide (for-syntax (all-from-out "version-misc.rkt")))
(provide version-case)


(define-for-syntax usage-message "Usage: (version-case [test code] ... [else ...]))")

(define-syntax (version-case stx)
  (syntax-case stx (else)
    [(_ [test code ...] ... [-else last-code ...])
     (and (not (null? (syntax->list (syntax ((test code ...) ...)))))
          (identifier? #'-else)
          (symbol=? (syntax-e #'-else) 'else))
     (with-syntax ([name (syntax/loc stx the-macro)]
                   [transformer
                    (syntax/loc stx
                      (lambda (stx*)
                        (cond [test
                               (syntax-local-introduce
                                (quote-syntax (begin code ...)))]
                              ...
                              [else 
                               (syntax-local-introduce
                                (quote-syntax (begin last-code ...)))])))])
       (case (syntax-local-context)
         [(expression)
          (syntax/loc stx
            (let-syntax ([name transformer])
              (name)))]
         [else
          (syntax/loc stx
            (begin
              (define-syntax name transformer)
              (name)))]))]
    [else
     (raise-syntax-error 
      #f 
      usage-message
      stx)]))