#lang racket

(require "il-structs.rkt"
         "simulator-structs.rkt"
         "simulator.rkt")


(define-syntax (test stx)
  (syntax-case stx ()
    [(_ actual exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (let ([results actual])
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


(let ([m (new-machine `(hello world ,(make-GotoStatement (make-Label 'hello))))])
  (test (machine-pc (step-n m 0)) 0)
  (test (machine-pc (step-n m 1)) 1)
  (test (machine-pc (step-n m 2)) 2)
  (test (machine-pc (step-n m 3)) 1)
  (test (machine-pc (step-n m 4)) 2)
  (test (machine-pc (step-n m 5)) 1))


(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const 42))))])
  (test (machine-val m) (void))
  (test (machine-val (step m)) 42))