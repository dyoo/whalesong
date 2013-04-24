#lang s-exp "../kernel.rkt"
(require (for-syntax racket/base
                     syntax/parse))

(provide with-handlers)

;; We adapt much of this from Racket's standard implementation of
;; with-handlers in racket/private/more-scheme.

(define (check-with-handlers-in-context handler-prompt-key)
  (unless (continuation-prompt-available? handler-prompt-key) 
    (error 'with-handlers
           "exception handler used out of context")))


(define handler-prompt-key (make-continuation-prompt-tag))


(define (call-handled-body handle-proc body-thunk)
  (call-with-continuation-prompt
   (lambda (body-thunk)
     ;; Restore the captured break parameterization for
     ;;  evaluating the `with-handlers' body. In this
     ;;  special case, no check for breaks is needed,
     ;;  because bpz is quickly restored past call/ec.
     ;;  Thus, `with-handlers' can evaluate its body in
     ;;  tail position.
     (with-continuation-mark 
       exception-handler-key
       (lambda (e)
         ;; Deliver the exception to the escape handler:
         (abort-current-continuation
          handler-prompt-key
          e))
       (body-thunk)))
   handler-prompt-key
   handle-proc
   body-thunk))



(define (select-handler e l)
  (let loop ([l l])
    (cond
     [(null? l)
      (raise e)]
     [((caar l) e)
      ((cdar l) e)]
     [else
       (loop (cdr l))])))



(define-syntax with-handlers
   (lambda (stx)
      (syntax-case stx ()
        [(_ () expr1 expr ...) (syntax/loc stx (let () expr1 expr ...))]
        [(_ ([pred handler] ...) expr1 expr ...)
         (with-syntax ([(pred-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-predicate) 
                                                                   (syntax->list #'(pred ...))))]
                       [(handler-name ...) (generate-temporaries (map (lambda (x) 'with-handlers-handler) 
                                                                      (syntax->list #'(handler ...))))])
           (syntax-protect
            (quasisyntax/loc stx
              (let-values ([(pred-name) pred] ...
                           [(handler-name) handler] ...)
                (call-handled-body
                 (lambda (e)
                   (select-handler e (list (cons pred-name handler-name) ...)))
                 (lambda ()
                   expr1 expr ...))))))])))
