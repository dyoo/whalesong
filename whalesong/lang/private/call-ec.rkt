#lang s-exp "../kernel.rkt"

(require (for-syntax racket/base
                     syntax/parse))

(provide call-with-escape-continuation
         call/ec
         let/ec)


(define (call-with-escape-continuation proc)
  (define p (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (lambda ()
     (proc (lambda args
             (abort-current-continuation p (lambda ()
                                             (apply values args))))))))

(define call/ec (procedure-rename call-with-escape-continuation 'call/ec))


(define-syntax (let/ec stx)
  (syntax-parse stx
    [(_ name:id body:expr ...+)
     (syntax/loc stx
       (call-with-escape-continuation (lambda (name)
                                        body ...)))]))
