#lang racket

(require "il-structs.rkt"
         "lexical-structs.rkt"
         "simulator-structs.rkt"
         "simulator-primitives.rkt"
         "simulator.rkt")


(define-syntax (test stx)
  (syntax-case stx ()
    [(_ actual exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ..." (syntax->datum #'stx))
           (let ([results actual])
             (unless (equal? results exp)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp results)
                                   #'stx)))
           (printf "ok\n\n"))))]))


;; take n steps in evaluating the machine.
(define (step-n m n)
  (cond
    [(= n 0)
     m]
    [else
     (step! m)
     (step-n m (sub1 n))]))


;; run: machine -> machine
;; Run the machine to completion.
(define (run m)
  (cond
    [(can-step? m)
     (step! m)
     (run m)]
    [else
     m]))


;; Infinite loop
(let ([m (new-machine `(hello world ,(make-GotoStatement (make-Label 'hello)))
                      #f)])
  (test (machine-pc (step-n m 0)) 0)
  (test (machine-pc (step-n m 1)) 1)
  (test (machine-pc (step-n m 1)) 2)
  (test (machine-pc (step-n m 1)) 1)
  (test (machine-pc (step-n m 1)) 2)
  (test (machine-pc (step-n m 1)) 1))


;; Assigning to val
(let ([m (new-machine `(,(make-AssignImmediateStatement 'val (make-Const 42)))
                      #f)])
  (test (machine-val m) (make-undefined))
  (step! m)
  (test (machine-val m) 42))

;; Assigning to proc
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const 42)))
                      #f)])
  (test (machine-proc m) (make-undefined))
  (step! m)
  (test (machine-proc m) 42))


;; Assigning to a environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 1 #f)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 42)))
                       #f)]
       [m (run m)])
  (test (machine-env m) '(42)))

;; Assigning to a boxed environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 1 #t)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #t) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) (list (box 42))))



;; Copying boxes over
(let* ([m (new-machine `(,(make-PushEnvironment 1 #t)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #t) (make-Const 42))
                         ,(make-PushEnvironment 1 #f)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                         (make-EnvLexicalReference 1 #t))))]
       [m (run m)])
  (test (machine-env m) (list 42 (box 42))))

(let* ([m (new-machine `(,(make-PushEnvironment 1 #t)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #t) (make-Const 42))
                         ,(make-PushEnvironment 1 #f)))]
       [m (run m)])
  (test (machine-env m) (list (make-undefined)
                              (box 42))))
(let* ([m (new-machine `(,(make-PushEnvironment 1 #t)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #t) (make-Const 42))
                         ,(make-PushEnvironment 1 #f)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                         (make-EnvLexicalReference 1 #f))))]
       [m (run m)])
  (test (machine-env m) (list (box 42)
                              (box 42))))




;; Assigning to another environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 2 #f)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) `(,(make-undefined) 42)))


;; Assigning to another environment reference
(let* ([m (new-machine `(,(make-PushEnvironment 2 #f)
                         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 42))))]
       [m (run m)])
  (test (machine-env m) `(42 ,(make-undefined))))


;; PushEnv
(let ([m (new-machine `(,(make-PushEnvironment 20 #f)))])
  (test (machine-env (run m)) (build-list 20 (lambda (i) (make-undefined)))))


;; PopEnv
(let ([m (new-machine `(,(make-PushEnvironment 20 #f)
                        ,(make-PopEnvironment 20 0)))])
  (test (machine-env (run m)) '()))

(let* ([m (new-machine `(,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const "louie"))
                        ,(make-PopEnvironment 1 0)))])
  (test (machine-env (run m)) '("dewey" "louie")))

(let* ([m (new-machine `(,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const "louie"))
                        ,(make-PopEnvironment 1 1)))])
  (test (machine-env (run m)) '("hewie" "louie")))

(let* ([m (new-machine `(,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const "louie"))
                        ,(make-PopEnvironment 1 2)))])
  (test (machine-env (run m)) '("hewie" "dewey")))

(let* ([m (new-machine `(,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const "hewie"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const "dewey"))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const "louie"))
                        ,(make-PopEnvironment 2 1)))])
  (test (machine-env (run m)) '("hewie")))



;; PushControl
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const #f))
                        foo 
                        ,(make-PushControlFrame 'foo)
                        bar
                        ,(make-PushControlFrame 'bar)
                        baz
                        ))])
  (test (machine-control (run m))
        (list (make-CallFrame 'bar #f)
              (make-CallFrame 'foo #f))))



;; PopControl
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const #f))
                        foo 
                        ,(make-PushControlFrame 'foo)
                        bar
                        ,(make-PushControlFrame 'bar)
                        baz
                        ,(make-PopControlFrame)
                        ))])
  (test (machine-control (run m))
        (list (make-CallFrame 'foo #f))))

(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const #f))
                        foo 
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
  (test (first (machine-env (run m)))
        (make-toplevel '(+ - * =)
                       (list (lookup-primitive '+)
                             (lookup-primitive '-)
                             (lookup-primitive '*)
                             (lookup-primitive '=)))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val))))])
  (test (machine-env (run m))
        (list (make-toplevel '(some-variable) (list "Danny")))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable another)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 1) (make-Reg 'val))))])
  (test (machine-env (run m))
        (list (make-toplevel '(some-variable another) (list (make-undefined) "Danny")))))

(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-PushEnvironment 5 #f)
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 5 0) (make-Reg 'val))))])
  (test (machine-env (run m))
        (list (make-undefined) (make-undefined) (make-undefined) (make-undefined) (make-undefined)
              (make-toplevel '(some-variable) (list "Danny")))))




;; check-toplevel-bound
;; This should produce an error.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-PerformStatement (make-CheckToplevelBound! 0 0))))])
  (with-handlers ((exn:fail? (lambda (exn)
                               (void))))
    
    (run m)
    (raise "I expected an error")))

;; check-toplevel-bound shouldn't fail here.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                        ,(make-AssignImmediateStatement 'val (make-Const "Danny"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val))
                        ,(make-PerformStatement (make-CheckToplevelBound! 0 0))))])
  (void (run m)))



;; install-closure-values
(let ([m  
       (make-machine (make-undefined) (make-closure 'procedure-entry
                                                    0
                                                    (list 1 2 3)
                                                    'procedure-entry)
                     (list true false) ;; existing environment holds true, false
                     '() 
                     0 
                     (list->vector `(,(make-PerformStatement (make-InstallClosureValues!))
                                     procedure-entry))
                     0
                     (make-hash))])
  (test (machine-env (run m))
        ;; Check that the environment has installed the expected closure values.
        (list 1 2 3 true false)))


;; get-compiled-procedure-entry
(let ([m 
       (make-machine (make-undefined) 
                     (make-closure 'procedure-entry 0 (list 1 2 3) 'procedure-entry)
                     (list true false) ;; existing environment holds true, false
                     '() 
                     0 
                     (list->vector `(,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))))
                     0
                     (make-hash))])
  (test (machine-val (run m))
        'procedure-entry))


;; make-compiled-procedure, with empty closure set
(let ([m (new-machine `(,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry 0 (list) 'procedure-entry))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry 0 (list) 'procedure-entry)))

;; make-compiled-procedure: Capturing a few variables.
(let ([m (new-machine `(,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const 'moe))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry 
                                                      0 
                                                      (list 0 2)
                                                      'procedure-entry))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry 0 (list 'larry 'moe)
                      'procedure-entry)))

;; make-compiled-procedure: Capturing a toplevel.
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(x y z)))
                        ,(make-AssignImmediateStatement 'val (make-Const "x"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val))
                        ,(make-AssignImmediateStatement 'val (make-Const "y"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 1) (make-Reg 'val))
                        ,(make-AssignImmediateStatement 'val (make-Const "z"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 2) (make-Reg 'val))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry 
                                                      0
                                                      (list 0)
                                                      'procedure-entry))
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry 0 (list (make-toplevel '(x y z) (list "x" "y" "z")))
                      'procedure-entry)))

;; make-compiled-procedure: Capturing both a toplevel and some lexical values
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(x y z)))
                        ,(make-AssignImmediateStatement 'val (make-Const "x"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val))
                        ,(make-AssignImmediateStatement 'val (make-Const "y"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 1) (make-Reg 'val))
                        ,(make-AssignImmediateStatement 'val (make-Const "z"))
                        ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 2) (make-Reg 'val))

                        ,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const 'moe))
                        ,(make-AssignPrimOpStatement 
                          'val
                          (make-MakeCompiledProcedure 'procedure-entry 
                                                      0
                                                      (list 3 0 2)
                                                      'procedure-entry))
                        ,(make-PopEnvironment 3 0)
                        ,(make-GotoStatement (make-Label 'end))
                        procedure-entry
                        end
                        ))])
  (test (machine-val (run m))
        (make-closure 'procedure-entry 
                      0
                      (list (make-toplevel '(x y z) (list "x" "y" "z"))
                            'larry
                            'moe)
                      'procedure-entry)))


;; Test toplevel lookup
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-AssignImmediateStatement 'val (make-EnvPrefixReference 0 0))))])
  (test (machine-val (run m))
        (lookup-primitive '+)))

;; Test lexical lookup
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const 'moe))
                        
                        ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))))])
  (test (machine-val (run m))
        'larry))
;; Another lexical lookup test
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-PushEnvironment 3 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 'larry))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 'curly))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f) (make-Const 'moe))
                        
                        ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 1 #f))))])
  (test (machine-val (run m))
        'curly))

;; ApplyPrimitiveProcedure
;; Adding two numbers
(let ([m (new-machine `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                        ,(make-AssignImmediateStatement 'proc (make-EnvPrefixReference 0 0))
                        ,(make-PushEnvironment 2 #f)
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f) (make-Const 126389))
                        ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f) (make-Const 42))
                        ,(make-AssignPrimOpStatement 'val (make-ApplyPrimitiveProcedure 2))
                        after))])
  (test (machine-val (run m))
        (+ 126389 42))
  
  (test (machine-env (run m))
        (list 126389 42 (make-toplevel '(+) (list (lookup-primitive '+))))))


;; GetControlStackLabel
(let ([m (new-machine `(,(make-AssignImmediateStatement 'proc (make-Const #f))
                        foo
                        ,(make-PushControlFrame 'foo)
                        ,(make-AssignPrimOpStatement 'proc (make-GetControlStackLabel))))])
  (test (machine-proc (run m))
        'foo))