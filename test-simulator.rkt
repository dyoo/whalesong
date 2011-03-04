#lang racket

(require "il-structs.rkt"
         "simulator-structs.rkt"
         "simulator-prims.rkt"
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


;; PushEnv
(let ([m (new-machine `(,(make-PushEnvironment 20)))])
  (test (machine-env (run m)) (build-list 20 (lambda (i) (void)))))


;; PopEnv
(let ([m (new-machine `(,(make-PushEnvironment 20)
                        ,(make-PopEnvironment 20 0)))])
  (test (machine-env (run m)) '()))

(let* ([m (new-machine `(,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const "louie"))
                        ,(make-PopEnvironment 1 0)))])
  (test (machine-env (run m)) '("dewey" "louie")))

(let* ([m (new-machine `(,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const "louie"))
                        ,(make-PopEnvironment 1 1)))])
  (test (machine-env (run m)) '("hewie" "louie")))

(let* ([m (new-machine `(,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const "louie"))
                        ,(make-PopEnvironment 1 2)))])
  (test (machine-env (run m)) '("hewie" "dewey")))

(let* ([m (new-machine `(,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const "louie"))
                        ,(make-PopEnvironment 2 1)))])
  (test (machine-env (run m)) '("hewie")))



;; PushControl
(let ([m (new-machine `(foo 
                        ,(make-PushControlFrame 'foo)
                        bar
                        ,(make-PushControlFrame 'bar)
                        baz
                        ))])
  (test (machine-control (run m))
        (list (make-frame 'bar)
              (make-frame 'foo))))



;; PopControl
(let ([m (new-machine `(foo 
                        ,(make-PushControlFrame 'foo)
                        bar
                        ,(make-PushControlFrame 'bar)
                        baz
                        ,(make-PopControlFrame)
                        ))])
  (test (machine-control (run m))
        (list (make-frame 'foo))))

(let ([m (new-machine `(foo 
                        ,(make-PushControlFrame 'foo)
                        bar
                        ,(make-PushControlFrame 'bar)
                        baz
                        ,(make-PopControlFrame)
                        ,(make-PopControlFrame)))])
  (test (machine-control (run m))
        (list)))





;; TestAndBranch: try the true branch
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const 42))
                        ,(make-TestAndBranchStatement 'false? 'val 'on-false)
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        ,(make-GotoStatement (make-Label 'end))
                        on-false
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                        end))])
  (test (machine-val (run m))
        'ok))
;; TestAndBranch: try the false branch
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const #f))
                        ,(make-TestAndBranchStatement 'false? 'val 'on-false)
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                        ,(make-GotoStatement (make-Label 'end))
                        on-false
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        end))])
  (test (machine-val (run m))
        'ok))
;; Test for primitive procedure
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const '+))
                        ,(make-TestAndBranchStatement 'primitive-procedure? 'val 'on-true)
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        ,(make-GotoStatement (make-Label 'end))
                        on-true
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                        end))])
  (test (machine-val (run m))
        'ok))
;; Give a primitive procedure in val
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const (lookup-primitive '+)))
                        ,(make-TestAndBranchStatement 'primitive-procedure? 'val 'on-true)
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                        ,(make-GotoStatement (make-Label 'end))
                        on-true
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        end))])
  (test (machine-val (run m))
        'ok))
;; Give a primitive procedure in proc, but test val
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const (lookup-primitive '+)))
                        ,(make-TestAndBranchStatement 'primitive-procedure? 'val 'on-true)
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-a-procedure))
                        ,(make-GotoStatement (make-Label 'end))
                        on-true
                        ,(make-AssignImmediateStatement 'val (make-Const 'a-procedure))
                        end))])
  (test (machine-val (run m))
        'not-a-procedure))
;; Give a primitive procedure in proc and test proc
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const (lookup-primitive '+)))
                        ,(make-TestAndBranchStatement 'primitive-procedure? 'proc 'on-true)
                        ,(make-AssignImmediateStatement 'val (make-Const 'not-a-procedure))
                        ,(make-GotoStatement (make-Label 'end))
                        on-true
                        ,(make-AssignImmediateStatement 'val (make-Const 'a-procedure))
                        end))])
  (test (machine-val (run m))
        'a-procedure))