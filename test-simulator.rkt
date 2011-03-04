#lang racket

(require "il-structs.rkt"
         "simulator-structs.rkt"
         "simulator.rkt")


(define-syntax (test stx)
  (syntax-case stx ()
    [(_ actual exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (let ([results (with-handlers ([exn:fail?
                                         (lambda (exn)
                                           (raise-syntax-error #f (format "Exception happened: ~s" 
                                                                          (exn-message exn))
                                                               #'stx))])
                          actual)])
           (unless (equal? actual exp)
             (raise-syntax-error #f (format "Expected ~s, got ~s" exp results)
                                 #'stx)))))]))


;; take n steps in evaluating the machine.
(define (step-n m n)
  (cond
    [(= n 0)
     m]
    [else
     (step-n (step m) (sub1 n))]))


;; run: machine -> machine
;; Run the machine to completion.
(define (run m)
  (cond
    [(can-step? m)
     (run (step m))]
    [else
     m]))


(let ([m (new-machine `(hello world ,(make-GotoStatement (make-Label 'hello))))])
  (test (machine-pc (step-n m 0)) 0)
  (test (machine-pc (step-n m 1)) 1)
  (test (machine-pc (step-n m 2)) 2)
  (test (machine-pc (step-n m 3)) 1)
  (test (machine-pc (step-n m 4)) 2)
  (test (machine-pc (step-n m 5)) 1))


;; Assigning to val
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const 42))))])
  (test (machine-val m) (void))
  (test (machine-val (step m)) 42))

;; Assigning to proc
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const 42))))])
  (test (machine-proc m) (void))
  (test (machine-proc (step m)) 42))


;; Assigning to a environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 1)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) '(42)))


;; Assigning to another environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 2)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) `(,(void) 42)))


;; Assigning to another environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 2)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) `(42 ,(void))))
