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
  (test (machine-val m) (make-undefined))
  (test (machine-val (step m)) 42))

;; Assigning to proc
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const 42))))])
  (test (machine-proc m) (make-undefined))
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
  (test (machine-env m) `(,(make-undefined) 42)))


;; Assigning to another environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 2)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) `(42 ,(make-undefined))))


;; PushEnv
(let ([m (new-machine `(,(make-PushEnvironment 20)))])
  (test (machine-env (run m)) (build-list 20 (lambda (i) (make-undefined)))))


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





;; AssignPrimOpStatement
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+ - * =)))))])
  ;; FIXME:  I'm hitting what appears to be a Typed Racket bug that prevents me from inspecting
  ;; the toplevel structure in the environment... :(
  (test (first (machine-env (run m)))
        (make-toplevel (list (lookup-primitive '+)
                             (lookup-primitive '-)
                             (lookup-primitive '*)
                             (lookup-primitive '=)))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-PerformStatement (make-SetToplevel! 0 0 'some-variable))))])
  (test (machine-env (run m))
        (list (make-toplevel (list "Danny")))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable another)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-PerformStatement (make-SetToplevel! 0 1 'another))))])
  (test (machine-env (run m))
        (list (make-toplevel (list (make-undefined) "Danny")))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-PushEnvironment 5)
                        ,(make-PerformStatement (make-SetToplevel! 5 0 'some-variable))))])
  (test (machine-env (run m))
        (list (make-undefined) (make-undefined) (make-undefined) (make-undefined) (make-undefined)
              (make-toplevel (list "Danny")))))




;; check-toplevel-bound
;; This should produce an error.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-PerformStatement (make-CheckToplevelBound! 0 0 'some-variable))))])
  (with-handlers ((exn:fail? (lambda (exn)
                               (void))))
    
    (run m)
    (raise "I expected an error")))

;; check-toplevel-bound shouldn't fail here.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-PerformStatement (make-SetToplevel! 0 0 'some-variable))
                        ,(make-PerformStatement (make-CheckToplevelBound! 0 0 'some-variable))))])
  (void (run m)))



;; install-closure-values
(let ([m  
       (make-machine (make-undefined) (make-closure 'procedure-entry
                                                    (list 1 2 3))
                     (list true false) ;; existing environment holds true, false
                     '() 
                     0 
                     (list->vector `(,(make-PerformStatement (make-InstallClosureValues!))
                                     procedure-entry
                                     )))])
  (test (machine-env (run m))
        ;; Check that the environment has installed the expected closure values.
        (list 1 2 3 true false)))


;; get-compiled-procedure-entry
(let ([m 
       (make-machine (make-undefined) 
                     (make-closure 'procedure-entry (list 1 2 3))
                     (list true false) ;; existing environment holds true, false
                     '() 
                     0 
                     (list->vector `(,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry)))))])
  (test (machine-val (run m))
        'procedure-entry))


;; make-compiled-procedure, with empty closure set
(let ([m (new-machine `(,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry (list)))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry (list))))

;; make-compiled-procedure: Capturing a few variables.
(let ([m (new-machine `(,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const 'moe))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry (list (make-EnvLexicalReference 0)
                                                                             (make-EnvLexicalReference 2))))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry (list 'larry 'moe))))

;; make-compiled-procedure: Capturing a toplevel.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(x y z)))
                        ,(make-AssignImmediateStatement 'val (make-Const "x"))
                        ,(make-PerformStatement (make-SetToplevel! 0 0 'x))
                        ,(make-AssignImmediateStatement 'val (make-Const "y"))
                        ,(make-PerformStatement (make-SetToplevel! 0 1 'y))
                        ,(make-AssignImmediateStatement 'val (make-Const "z"))
                        ,(make-PerformStatement (make-SetToplevel! 0 2 'z))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry (list (make-EnvWholePrefixReference 0))))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry (list (make-toplevel (list "x" "y" "z"))))))

;; make-compiled-procedure: Capturing both a toplevel and some lexical values
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(x y z)))
                        ,(make-AssignImmediateStatement 'val (make-Const "x"))
                        ,(make-PerformStatement (make-SetToplevel! 0 0 'x))
                        ,(make-AssignImmediateStatement 'val (make-Const "y"))
                        ,(make-PerformStatement (make-SetToplevel! 0 1 'y))
                        ,(make-AssignImmediateStatement 'val (make-Const "z"))
                        ,(make-PerformStatement (make-SetToplevel! 0 2 'z))

                        ,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const 'moe))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry (list (make-EnvWholePrefixReference 3)
                                                                             (make-EnvLexicalReference 0)
                                                                             (make-EnvLexicalReference 2))))
                        ,(make-PopEnvironment 3 0)
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry (list (make-toplevel (list "x" "y" "z"))
                                             'larry
                                             'moe))))


;; Test toplevel lookup
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-AssignPrimOpStatement 'val (make-LookupToplevelAddress 0 0 '+))))])
  (test (machine-val (run m))
        (lookup-primitive '+)))

;; Test lexical lookup
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const 'moe))
                        
                        ,(make-AssignPrimOpStatement 'val (make-LookupLexicalAddress 0))))])
  (test (machine-val (run m))
        'larry))
;; Another lexical lookup test
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-PushEnvironment 3)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2) (make-Const 'moe))
                        
                        ,(make-AssignPrimOpStatement 'val (make-LookupLexicalAddress 1))))])
  (test (machine-val (run m))
        'curly))



#;(let ([m (new-machine `(,(make-AssignPrimOpStatement (make-LookupLexicalAddress))))])
  (test ...))
#;(let ([m (new-machine `(,(make-AssignPrimOpStatement (make-LookupToplevelAddress))))])
  (test ...))
#;(let ([m (new-machine `(,(make-AssignPrimOpStatement (make-GetControlStackLabel))))])
  (test ...))