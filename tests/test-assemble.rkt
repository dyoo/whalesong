#lang racket

(require "browser-evaluate.rkt"
         "../js-assembler/assemble.rkt"
         "../js-assembler/package.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/il-structs.rkt"
         racket/port
         racket/promise
         racket/runtime-path)

(printf "test-assemble.rkt\n")

(define runtime (get-runtime))


; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expr expected)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" (syntax->datum #'expr))
           (let ([actual 
                  (with-handlers ([void
                                   (lambda (exn)
                                     (raise-syntax-error #f (format "Runtime error: got ~s" exn)
                                                         #'stx))])
                    expr)])
             (unless (equal? actual expected)
               (raise-syntax-error #f (format "Expected ~s, got ~s" expected actual)
                                   #'stx))
             (printf "ok.\n\n")))))]))


;; evaluating single expression
(define -E (delay (make-evaluate
                   (lambda (a-statement+inspector op)
                     (let* ([a-statement (car a-statement+inspector)]
                            [inspector (cdr a-statement+inspector)]
                            [snippet (assemble-statement a-statement)]
                            [code
                             (string-append
                              "(function() { "

                              runtime
                              "var RUNTIME = plt.runtime;"
                              "var MACHINE = new plt.runtime.Machine();\n"

                              "return function(success, fail, params){" 
			      snippet
                              (format "success(plt.runtime.toDisplayedString(~a)); };" inspector)
                              "});")])
                       (displayln snippet)
                       (display code op))))))
(define (E-single a-statement (inspector "MACHINE.val"))
  (evaluated-value ((force -E) (cons a-statement inspector))))

;; evaluating many expressions[.
(define -E-many (delay (make-evaluate
                        (lambda (a-statement+inspector op)
                          (let* ([a-statement (car a-statement+inspector)]
                                 [inspector (cdr a-statement+inspector)])

                            (display runtime op)
                            "var RUNTIME = plt.runtime;"
                            (display "var MACHINE = new plt.runtime.Machine();\n" op)                           
                            (display "(function() { " op)
                            (display "var myInvoke = " op)
                            (assemble/write-invoke a-statement op)
                            (display ";" op)
                            
                            (fprintf op 
                                     "return function(succ, fail, params) { myInvoke(MACHINE, function(v) { succ(plt.runtime.toDisplayedString(~a));}, fail, params); }"
                                     inspector)
                            (display "})" op))))))
(define (E-many stmts (inspector "MACHINE.val"))
  (evaluated-value ((force -E-many) (cons stmts inspector))))








;; Assigning a number
(test (E-single (make-AssignImmediateStatement 'val (make-Const 42)))
      "42")
;; Assigning a string
(test (E-single (make-AssignImmediateStatement 'val (make-Const "Danny")))
      "Danny")
;; Assigning a cons
(test (E-single (make-AssignImmediateStatement 'val (make-Const (cons 1 2))))
      "(1 . 2)")
;; Assigning a void
(test (E-single (make-AssignImmediateStatement 'val (make-Const (void))))
      "#<void>")
;; Assigning to proc means val should still be uninitialized.
(test (E-single (make-AssignImmediateStatement 'proc (make-Const "Danny")))
      "#<undefined>")
;; But we should see the assignment if we inspect MACHINE.proc.
(test (E-single (make-AssignImmediateStatement 'proc (make-Const "Danny"))
                "MACHINE.proc")
      "Danny")


(test (E-single (make-PushEnvironment 1 #f)
                "MACHINE.env.length")
      "1")
(test (E-single (make-PushEnvironment 20 #f)
                "MACHINE.env.length")
      "20")

;; PopEnvironment
(test (E-many (list (make-PushEnvironment 2 #f))
              "MACHINE.env.length")
      "2")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-PopEnvironment (make-Const 1) 
                                         (make-Const 0)))
              "MACHINE.env.length")
      "1")



;; Assigning to the environment
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const 12345)))
              "MACHINE.env[1]")
      "12345")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const 12345)))
              "MACHINE.env[0]")
      "#<undefined>")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const 12345)))
              "MACHINE.env[0]")
      "12345")


;; Toplevel Environment loading
(test (E-single (make-PerformStatement (make-ExtendEnvironment/Prefix! '(pi)))
                "plt.runtime.toWrittenString(MACHINE.env[0]).slice(0, 5)")
      "3.141")



;; Simple application
(test (E-many (list (make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                    (make-AssignImmediateStatement 'proc (make-EnvPrefixReference 0 0))
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const 3))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const 4))
                    (make-AssignImmediateStatement 'argcount (make-Const 2))
                    (make-AssignPrimOpStatement 'val (make-ApplyPrimitiveProcedure))
                    'done))
      "7")




;; A do-nothing closure
(test (E-many (list (make-GotoStatement (make-Label 'afterLambda))
                    'closureStart
                    (make-GotoStatement (make-Label 'afterLambda))
                    'afterLambda
                    (make-AssignPrimOpStatement 'val (make-MakeCompiledProcedure 'closureStart 0 '() 'closureStart)))
              "MACHINE.val.displayName")
      "closureStart")


;; A do-nothing closure with a few values
(test (E-many (list (make-GotoStatement (make-Label 'afterLambda))
                    'closureStart
                    (make-GotoStatement (make-Label 'afterLambda))
                    'afterLambda
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOpStatement 'val (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart)))
              "MACHINE.val.closedVals[1] + ',' + MACHINE.val.closedVals[0]")
      "hello,world")

;; Let's try to install the closure values.
(test (E-many (list (make-GotoStatement (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-PerformStatement (make-InstallClosureValues!))
                    (make-GotoStatement (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOpStatement 'proc (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart))
                    (make-PopEnvironment (make-Const 2) 
                                         (make-Const 0))
                    (make-GotoStatement (make-Label 'closureStart))
                    'theEnd)
              "plt.runtime.toWrittenString(MACHINE.env.length) + ',' + MACHINE.env[1] + ',' + MACHINE.env[0]")
      "2,hello,world")



;; get-compiled-procedure-entry
(test (E-many (list (make-GotoStatement (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-PerformStatement (make-InstallClosureValues!))
                    (make-GotoStatement (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOpStatement 'proc (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart))
                    (make-PopEnvironment (make-Const 2) (make-Const 0))
                    (make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry)))
              "typeof(MACHINE.val) + ',' + (MACHINE.val === MACHINE.proc.label)")
      "function,true")


;; check-closure-arity.  This should succeed.
(void (E-many (list (make-GotoStatement (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-PerformStatement (make-InstallClosureValues!))
                    (make-GotoStatement (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOpStatement 'proc (make-MakeCompiledProcedure 'closureStart 5 
                                                                                  (list 0 1)
                                                                                  'closureStart))
                    (make-PopEnvironment (make-Const 2) (make-Const 0))
                    (make-PerformStatement (make-CheckClosureArity! (make-Const 5))))))

;; this should fail, since the check is for 1, but the closure expects 5.
(let/ec return
  (with-handlers ([void
                   (lambda (exn) (return))])
    (E-many (list (make-GotoStatement (make-Label 'afterLambdaBody))
                  
                  'closureStart
                  (make-PerformStatement (make-InstallClosureValues!))
                  (make-GotoStatement (make-Label 'theEnd))
                  
                  'afterLambdaBody
                  (make-PushEnvironment 2 #f)
                  (make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                 (make-Const "hello"))
                  (make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                 (make-Const "world"))
                  (make-AssignPrimOpStatement 'proc (make-MakeCompiledProcedure 'closureStart 5 
                                                                                (list 0 1)
                                                                                'closureStart))
                  (make-PopEnvironment (make-Const 2) (make-Const 0))
                  (make-PerformStatement (make-CheckClosureArity! (make-Const 1))))))
  (error 'expected-failure))





(test (E-many `(,(make-AssignImmediateStatement 'val (make-Const 42))
                ,(make-TestAndBranchStatement (make-TestFalse (make-Reg 'val)) 'onFalse)
                ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                ,(make-GotoStatement (make-Label 'end))
                onFalse
                ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                end))
      "ok")

;; TestAndBranch: try the false branch
(test (E-many `(,(make-AssignImmediateStatement 'val (make-Const #f))
                ,(make-TestAndBranchStatement (make-TestFalse (make-Reg 'val)) 'onFalse)
                ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                ,(make-GotoStatement (make-Label 'end))
                onFalse
                ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                end))
      "ok")

;; Test for primitive procedure
(test (E-many `(,(make-AssignImmediateStatement 'val (make-Const '+))
                ,(make-TestAndBranchStatement (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
                ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                ,(make-GotoStatement (make-Label 'end))
                onTrue
                ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                end))
      "ok")

;; Give a primitive procedure in val
(test (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                ,(make-AssignImmediateStatement 'val (make-EnvPrefixReference 0 0))
                ,(make-TestAndBranchStatement (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
                ,(make-AssignImmediateStatement 'val (make-Const 'not-ok))
                ,(make-GotoStatement (make-Label 'end))
                onTrue
                ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                end))
      "ok")

;; Give a primitive procedure in proc, but test val
(test (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                ,(make-AssignImmediateStatement 'proc (make-EnvPrefixReference 0 0))
                ,(make-TestAndBranchStatement (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
                ,(make-AssignImmediateStatement 'val (make-Const 'not-a-procedure))
                ,(make-GotoStatement (make-Label 'end))
                onTrue
                ,(make-AssignImmediateStatement 'val (make-Const 'a-procedure))
                end))
      "not-a-procedure")

;; Give a primitive procedure in proc and test proc
(test (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(+)))
                ,(make-AssignImmediateStatement 'proc (make-EnvPrefixReference 0 0))
                ,(make-TestAndBranchStatement (make-TestPrimitiveProcedure (make-Reg 'proc)) 'onTrue)
                ,(make-AssignImmediateStatement 'val (make-Const 'not-a-procedure))
                ,(make-GotoStatement (make-Label 'end))
                onTrue
                ,(make-AssignImmediateStatement 'val (make-Const 'a-procedure))
                end))
      "a-procedure")



;; Set-toplevel
(test (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(advisor)))
                ,(make-AssignImmediateStatement 'val (make-Const "Kathi"))
                ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val)))
              "MACHINE.env[0][0]")
      "Kathi")


;; check-toplevel-bound
(let/ec return
  (let ([dont-care 
         (with-handlers ([void (lambda (exn) (return))])
           (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(some-variable)))
                     ,(make-PerformStatement (make-CheckToplevelBound! 0 0)))))])
    (raise "I expected an error")))
  
;; check-toplevel-bound shouldn't fail here.
(test (E-many `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! '(another-advisor)))
                ,(make-AssignImmediateStatement 'val (make-Const "Shriram"))
                ,(make-AssignImmediateStatement (make-EnvPrefixReference 0 0) (make-Reg 'val))
                ,(make-PerformStatement (make-CheckToplevelBound! 0 0)))
              "MACHINE.env[0][0]")
      "Shriram")



(test (E-many `(,(make-PushEnvironment 1 #f)
                ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                (make-Const '(1 2 3)))
                ,(make-AssignImmediateStatement 'argcount (make-Const 1))
                ,(make-PerformStatement (make-SpliceListIntoStack! (make-Const 0))))
              "MACHINE.argcount + ',' + MACHINE.env[0] + ',' + MACHINE.env[1] + ',' + MACHINE.env[2]")
      "3,3,2,1")


(test (E-many `(,(make-PushEnvironment 3 #f)
                ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                (make-Const "hello"))
                ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                                (make-Const "world"))
                ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f)
                                                (make-Const '(1 2 3)))
                ,(make-AssignImmediateStatement 'argcount (make-Const 3))
                ,(make-PerformStatement (make-SpliceListIntoStack! (make-Const 2))))
              "MACHINE.argcount + ',' + MACHINE.env[0] + ',' + MACHINE.env[1] + ',' + MACHINE.env[2] + ',' + MACHINE.env[3] + ',' + MACHINE.env[4]")
      "5,3,2,1,world,hello")







;; testing rest splicing
(test (E-many `(,(make-PushEnvironment 1 #f)
                ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                                (make-Const "hello"))
                ,(make-AssignImmediateStatement 'argcount (make-Const 1))
                ,(make-PerformStatement (make-UnspliceRestFromStack! (make-Const 0)
                                                                     (make-Const 1))))
              "MACHINE.argcount + ',' + plt.runtime.isList(MACHINE.env[0])")
      "1,true")


(test (E-many
       `(,(make-PushEnvironment 5 #f)
         ,(make-AssignImmediateStatement (make-EnvLexicalReference 0 #f)
                                         (make-Const "hello"))
         ,(make-AssignImmediateStatement (make-EnvLexicalReference 1 #f)
                                         (make-Const "world"))
         ,(make-AssignImmediateStatement (make-EnvLexicalReference 2 #f)
                                         (make-Const 'x))
         ,(make-AssignImmediateStatement (make-EnvLexicalReference 3 #f)
                                         (make-Const 'y))
         ,(make-AssignImmediateStatement (make-EnvLexicalReference 4 #f)
                                         (make-Const 'z))
         ,(make-AssignImmediateStatement 'argcount (make-Const 5))
         ,(make-PerformStatement (make-UnspliceRestFromStack! (make-Const 2)  (make-Const 3))))
       "MACHINE.argcount + ',' + MACHINE.env.length + ',' + plt.runtime.isList(MACHINE.env[0]) + ',' + MACHINE.env[2] + ',' + MACHINE.env[1]")
      "3,3,true,hello,world")



;; Check closure mismatch.  Make sure we're getting the right values from the test.
(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry 0 (list) 'procedure-entry))
                        ,(make-TestAndBranchStatement
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 0))
                          'bad)
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        ,(make-GotoStatement (make-Label 'end))
                        bad
                        ,(make-AssignImmediateStatement 'val (make-Const 'bad))
                        end)
              "MACHINE.val")
      "ok")
      

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry 0 (list) 'procedure-entry))
                        ,(make-TestAndBranchStatement
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 1))
                          'ok)
                        ,(make-AssignImmediateStatement 'val (make-Const 'bad))
                        ,(make-GotoStatement (make-Label 'end))
                        ok
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        end)
              "MACHINE.val")
      "ok")

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list) 'procedure-entry))
                        ,(make-TestAndBranchStatement
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 0))
                          'ok)
                        ,(make-AssignImmediateStatement 'val (make-Const 'bad))
                        ,(make-GotoStatement (make-Label 'end))
                        ok
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        end)
              "MACHINE.val")
      "ok")

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list) 'procedure-entry))
                        ,(make-TestAndBranchStatement
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 2))
                          'bad)
                        ,(make-AssignImmediateStatement 'val (make-Const 'ok))
                        ,(make-GotoStatement (make-Label 'end))
                        bad
                        ,(make-AssignImmediateStatement 'val (make-Const 'bad))
                        end)
              "MACHINE.val")
      "ok")




;; Let's test closure value lookup.
(test (E-many `(,(make-PushImmediateOntoEnvironment (make-Const 3) #f)
                ,(make-PushImmediateOntoEnvironment (make-Const 4) #f)
                procedure-entry
                ;; doesn't matter about the procedure entry...
                ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list 0 1) 'procedure-entry))
                ,(make-AssignImmediateStatement 'val (make-CompiledProcedureClosureReference (make-Reg 'proc) 0)))
              "MACHINE.val")
      "4")

(test (E-many `(,(make-PushImmediateOntoEnvironment (make-Const 3) #f)
                ,(make-PushImmediateOntoEnvironment (make-Const 4) #f)
                procedure-entry
                ;; doesn't matter about the procedure entry...
                ,(make-AssignPrimOpStatement 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list 0 1) 'procedure-entry))
                ,(make-AssignImmediateStatement 'val (make-CompiledProcedureClosureReference (make-Reg 'proc) 1)))
              "MACHINE.val")
      "3")




