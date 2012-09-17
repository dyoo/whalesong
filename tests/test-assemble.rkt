#lang racket

(require (planet dyoo/browser-evaluate)
         "../js-assembler/assemble.rkt"
         "../js-assembler/package.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/arity-structs.rkt"
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
                            [snippet (assemble-statement a-statement (make-hash))]
                            [code
                             (string-append
                              "(function() { "

                              runtime
                              "var RT = plt.runtime;"
                              "var M = new plt.runtime.Machine();\n"

                              "return function(success, fail, params){" 
			      snippet
                              (format "success(plt.runtime.toDisplayedString(~a)); };" inspector)
                              "});")])
                       (displayln snippet)
                       (display code op))))))
(define (E-single a-statement (inspector "M.v"))
  (evaluated-value ((force -E) (cons a-statement inspector))))

;; evaluating many expressions[.
(define -E-many (delay (make-evaluate
                        (lambda (a-statement+inspector op)
                          (let* ([a-statement (car a-statement+inspector)]
                                 [inspector (cdr a-statement+inspector)])

                            (display runtime op)
                            "var RT = plt.runtime;"
                            (display "var M = new plt.runtime.Machine();\n" op)                           
                            (display "(function() { " op)
                            (display "var myInvoke = " op)
                            (assemble/write-invoke a-statement op)
                            (display ";" op)
                            (fprintf op 
                                     "return function(succ, fail, params) {
                                           var newParams = { currentDisplayer: function(M, v) {
                                                                    params.currentDisplayer(v); } };

                                           myInvoke(M,
                                                    function(v) { succ(plt.runtime.toDisplayedString(~a));},
                                                    function(M, exn) { fail(exn); },
                                                    newParams);
                                      }"
                                     inspector)
                            (display "})" op))))))
(define (E-many stmts (inspector "M.v"))
  (evaluated-value ((force -E-many) (cons stmts inspector))))








;; Assigning a number
(test (E-single (make-AssignImmediate 'val (make-Const 42)))
      "42")
;; Assigning a string
(test (E-single (make-AssignImmediate 'val (make-Const "Danny")))
      "Danny")
;; Assigning a cons
(test (E-single (make-AssignImmediate 'val (make-Const (cons 1 2))))
      "(1 . 2)")
;; Assigning a void
(test (E-single (make-AssignImmediate 'val (make-Const (void))))
      "#<void>")
;; Assigning to proc means val should still be uninitialized.
(test (E-single (make-AssignImmediate 'proc (make-Const "Danny")))
      "#<undefined>")
;; But we should see the assignment if we inspect M.proc.
(test (E-single (make-AssignImmediate 'proc (make-Const "Danny"))
                "M.p")
      "Danny")


(test (E-single (make-PushEnvironment 1 #f)
                "M.e.length")
      "1")
(test (E-single (make-PushEnvironment 20 #f)
                "M.e.length")
      "20")

;; PopEnvironment
(test (E-many (list (make-PushEnvironment 2 #f))
              "M.e.length")
      "2")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-PopEnvironment (make-Const 1) 
                                         (make-Const 0)))
              "M.e.length")
      "1")



;; Assigning to the environment
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const 12345)))
              "M.e[1]")
      "12345")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const 12345)))
              "M.e[0]")
      "#<undefined>")
(test (E-many (list (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                   (make-Const 12345)))
              "M.e[0]")
      "12345")


;; Toplevel Environment loading
(test (E-single (make-Perform (make-ExtendEnvironment/Prefix! '(pi)))
                "plt.runtime.toWrittenString(M.e[0]).slice(0, 5)")
      "3.141")



;; Simple application
;; (test (E-many (list (make-Perform (make-ExtendEnvironment/Prefix! '(+)))
;;                     (make-AssignImmediate 'proc (make-EnvPrefixReference 0 0 #f))
;;                     (make-PushEnvironment 2 #f)
;;                     (make-AssignImmediate (make-EnvLexicalReference 0 #f)
;;                                                    (make-Const 3))
;;                     (make-AssignImmediate (make-EnvLexicalReference 1 #f)
;;                                                    (make-Const 4))
;;                     (make-AssignImmediate 'argcount (make-Const 2))
;;                     (make-AssignPrimOp 'val (make-ApplyPrimitiveProcedure))
;;                     'done))
;;       "7")




;; A do-nothing closure
(test (E-many (list (make-Goto (make-Label 'afterLambda))
                    'closureStart
                    (make-Goto (make-Label 'afterLambda))
                    'afterLambda
                    (make-AssignPrimOp 'val (make-MakeCompiledProcedure 'closureStart 0 '() 'closureStart)))
              "M.v.displayName")
      "closureStart")


;; A do-nothing closure with a few values
(test (E-many (list (make-Goto (make-Label 'afterLambda))
                    'closureStart
                    (make-Goto (make-Label 'afterLambda))
                    'afterLambda
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOp 'val (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart)))
              "M.v.closedVals[1] + ',' + M.v.closedVals[0]")
      "hello,world")

;; Let's try to install the closure values.
(test (E-many (list (make-Goto (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-Perform (make-InstallClosureValues! 2))
                    (make-Goto (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOp 'proc (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart))
                    (make-PopEnvironment (make-Const 2) 
                                         (make-Const 0))
                    (make-Goto (make-Label 'closureStart))
                    'theEnd)
              "plt.runtime.toWrittenString(M.e.length) + ',' + M.e[1] + ',' + M.e[0]")
      "2,hello,world")



;; get-compiled-procedure-entry
(test (E-many (list (make-Goto (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-Perform (make-InstallClosureValues! 2))
                    (make-Goto (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOp 'proc (make-MakeCompiledProcedure 'closureStart 0 
                                                                                 (list 0 1)
                                                                                 'closureStart))
                    (make-PopEnvironment (make-Const 2) (make-Const 0))
                    (make-AssignPrimOp 'val (make-GetCompiledProcedureEntry))
                    'theEnd)
              "typeof(M.v) + ',' + (M.v === M.p.label)")
      "function,true")


;; check-closure-arity.  This should succeed.
(void (E-many (list (make-Goto (make-Label 'afterLambdaBody))
                    
                    'closureStart
                    (make-Perform (make-InstallClosureValues! 2))
                    (make-Goto (make-Label 'theEnd))

                    'afterLambdaBody
                    (make-PushEnvironment 2 #f)
                    (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                   (make-Const "hello"))
                    (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                   (make-Const "world"))
                    (make-AssignPrimOp 'proc (make-MakeCompiledProcedure 'closureStart 5 
                                                                                  (list 0 1)
                                                                                  'closureStart))
                    (make-PopEnvironment (make-Const 2) (make-Const 0))
                    (make-AssignImmediate 'argcount (make-Const 5))
                    (make-Perform (make-CheckClosureAndArity!))
                    'theEnd)))

;; this should fail, since the check is for 1, but the closure expects 5.
(let/ec return
  (with-handlers ([void
                   (lambda (exn) (return))])
    (E-many (list (make-Goto (make-Label 'afterLambdaBody))
                  
                  'closureStart
                  (make-Perform (make-InstallClosureValues! 2))
                  (make-Goto (make-Label 'theEnd))
                  
                  'afterLambdaBody
                  (make-PushEnvironment 2 #f)
                  (make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                 (make-Const "hello"))
                  (make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                 (make-Const "world"))
                  (make-AssignPrimOp 'proc (make-MakeCompiledProcedure 'closureStart 5 
                                                                                (list 0 1)
                                                                                'closureStart))
                  (make-PopEnvironment (make-Const 2) (make-Const 0))
                  (make-AssignImmediate 'argcount (make-Const 1))

                  (make-Perform (make-CheckClosureAndArity!))
                  'theEnd)))
  (error 'expected-failure))





(test (E-many `(,(make-AssignImmediate 'val (make-Const 42))
                ,(make-TestAndJump (make-TestFalse (make-Reg 'val)) 'onFalse)
                ,(make-AssignImmediate 'val (make-Const 'ok))
                ,(make-Goto (make-Label 'end))
                onFalse
                ,(make-AssignImmediate 'val (make-Const 'not-ok))
                end))
      "ok")

;; TestAndBranch: try the false branch
(test (E-many `(,(make-AssignImmediate 'val (make-Const #f))
                ,(make-TestAndJump (make-TestFalse (make-Reg 'val)) 'onFalse)
                ,(make-AssignImmediate 'val (make-Const 'not-ok))
                ,(make-Goto (make-Label 'end))
                onFalse
                ,(make-AssignImmediate 'val (make-Const 'ok))
                end))
      "ok")

;; ;; Test for primitive procedure
;; (test (E-many `(,(make-AssignImmediate 'val (make-Const '+))
;;                 ,(make-TestAndJump (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
;;                 ,(make-AssignImmediate 'val (make-Const 'ok))
;;                 ,(make-Goto (make-Label 'end))
;;                 onTrue
;;                 ,(make-AssignImmediate 'val (make-Const 'not-ok))
;;                 end))
;;       "ok")

;; ;; Give a primitive procedure in val
;; (test (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(+)))
;;                 ,(make-AssignImmediate 'val (make-EnvPrefixReference 0 0 #f))
;;                 ,(make-TestAndJump (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
;;                 ,(make-AssignImmediate 'val (make-Const 'not-ok))
;;                 ,(make-Goto (make-Label 'end))
;;                 onTrue
;;                 ,(make-AssignImmediate 'val (make-Const 'ok))
;;                 end))
;;       "ok")

;; ;; Give a primitive procedure in proc, but test val
;; (test (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(+)))
;;                 ,(make-AssignImmediate 'proc (make-EnvPrefixReference 0 0 #f))
;;                 ,(make-TestAndJump (make-TestPrimitiveProcedure (make-Reg 'val)) 'onTrue)
;;                 ,(make-AssignImmediate 'val (make-Const 'not-a-procedure))
;;                 ,(make-Goto (make-Label 'end))
;;                 onTrue
;;                 ,(make-AssignImmediate 'val (make-Const 'a-procedure))
;;                 end))
;;       "not-a-procedure")

;; ;; Give a primitive procedure in proc and test proc
;; (test (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(+)))
;;                 ,(make-AssignImmediate 'proc (make-EnvPrefixReference 0 0 #f))
;;                 ,(make-TestAndJump (make-TestPrimitiveProcedure (make-Reg 'proc)) 'onTrue)
;;                 ,(make-AssignImmediate 'val (make-Const 'not-a-procedure))
;;                 ,(make-Goto (make-Label 'end))
;;                 onTrue
;;                 ,(make-AssignImmediate 'val (make-Const 'a-procedure))
;;                 end))
;;       "a-procedure")



;; Set-toplevel
(test (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(advisor)))
                ,(make-AssignImmediate 'val (make-Const "Kathi"))
                ,(make-AssignImmediate (make-EnvPrefixReference 0 0 #f) (make-Reg 'val)))
              "M.e[0][0]")
      "Kathi")


;; check-toplevel-bound
(let/ec return
  (let ([dont-care 
         (with-handlers ([void (lambda (exn) (return))])
           (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(some-variable)))
                     ,(make-Perform (make-CheckToplevelBound! 0 0)))))])
    (raise "I expected an error")))
  
;; check-toplevel-bound shouldn't fail here.
(test (E-many `(,(make-Perform (make-ExtendEnvironment/Prefix! '(another-advisor)))
                ,(make-AssignImmediate 'val (make-Const "Shriram"))
                ,(make-AssignImmediate (make-EnvPrefixReference 0 0 #f) (make-Reg 'val))
                ,(make-Perform (make-CheckToplevelBound! 0 0)))
              "M.e[0][0]")
      "Shriram")



(test (E-many `(,(make-PushEnvironment 1 #f)
                ,(make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                (make-Const '(1 2 3)))
                ,(make-AssignImmediate 'argcount (make-Const 1))
                ,(make-Perform (make-SpliceListIntoStack! (make-Const 0))))
              "M.a + ',' + M.e[0] + ',' + M.e[1] + ',' + M.e[2]")
      "3,3,2,1")


(test (E-many `(,(make-PushEnvironment 3 #f)
                ,(make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                (make-Const "hello"))
                ,(make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                                (make-Const "world"))
                ,(make-AssignImmediate (make-EnvLexicalReference 2 #f)
                                                (make-Const '(1 2 3)))
                ,(make-AssignImmediate 'argcount (make-Const 3))
                ,(make-Perform (make-SpliceListIntoStack! (make-Const 2))))
              "M.a + ',' + M.e[0] + ',' + M.e[1] + ',' + M.e[2] + ',' + M.e[3] + ',' + M.e[4]")
      "5,3,2,1,world,hello")







;; testing rest splicing
(test (E-many `(,(make-PushEnvironment 1 #f)
                ,(make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                                (make-Const "hello"))
                ,(make-AssignImmediate 'argcount (make-Const 1))
                ,(make-Perform (make-UnspliceRestFromStack! (make-Const 0)
                                                                     (make-Const 1))))
              "M.a + ',' + plt.runtime.isList(M.e[0])")
      "1,true")


(test (E-many
       `(,(make-PushEnvironment 5 #f)
         ,(make-AssignImmediate (make-EnvLexicalReference 0 #f)
                                         (make-Const "hello"))
         ,(make-AssignImmediate (make-EnvLexicalReference 1 #f)
                                         (make-Const "world"))
         ,(make-AssignImmediate (make-EnvLexicalReference 2 #f)
                                         (make-Const 'x))
         ,(make-AssignImmediate (make-EnvLexicalReference 3 #f)
                                         (make-Const 'y))
         ,(make-AssignImmediate (make-EnvLexicalReference 4 #f)
                                         (make-Const 'z))
         ,(make-AssignImmediate 'argcount (make-Const 5))
         ,(make-Perform (make-UnspliceRestFromStack! (make-Const 2)  (make-Const 3))))
       "M.a + ',' + M.e.length + ',' + plt.runtime.isList(M.e[0]) + ',' + M.e[2] + ',' + M.e[1]")
      "3,3,true,hello,world")



;; Check closure mismatch.  Make sure we're getting the right values from the test.
(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry 0 (list) 'procedure-entry))
                        ,(make-TestAndJump
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 0))
                          'bad)
                        ,(make-AssignImmediate 'val (make-Const 'ok))
                        ,(make-Goto (make-Label 'end))
                        bad
                        ,(make-AssignImmediate 'val (make-Const 'bad))
                        end)
              "M.v")
      "ok")
      

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry 0 (list) 'procedure-entry))
                        ,(make-TestAndJump
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 1))
                          'ok)
                        ,(make-AssignImmediate 'val (make-Const 'bad))
                        ,(make-Goto (make-Label 'end))
                        ok
                        ,(make-AssignImmediate 'val (make-Const 'ok))
                        end)
              "M.v")
      "ok")

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list) 'procedure-entry))
                        ,(make-TestAndJump
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 0))
                          'ok)
                        ,(make-AssignImmediate 'val (make-Const 'bad))
                        ,(make-Goto (make-Label 'end))
                        ok
                        ,(make-AssignImmediate 'val (make-Const 'ok))
                        end)
              "M.v")
      "ok")

(test (E-many `(procedure-entry
                        ;; doesn't matter about the procedure entry...
                        ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list) 'procedure-entry))
                        ,(make-TestAndJump
                          (make-TestClosureArityMismatch (make-Reg 'proc) (make-Const 2))
                          'bad)
                        ,(make-AssignImmediate 'val (make-Const 'ok))
                        ,(make-Goto (make-Label 'end))
                        bad
                        ,(make-AssignImmediate 'val (make-Const 'bad))
                        end)
              "M.v")
      "ok")




;; Let's test closure value lookup.
(test (E-many `(,(make-PushImmediateOntoEnvironment (make-Const 3) #f)
                ,(make-PushImmediateOntoEnvironment (make-Const 4) #f)
                procedure-entry
                ;; doesn't matter about the procedure entry...
                ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list 0 1) 'procedure-entry))
                ,(make-AssignImmediate 'val (make-CompiledProcedureClosureReference (make-Reg 'proc) 0)))
              "M.v")
      "4")

(test (E-many `(,(make-PushImmediateOntoEnvironment (make-Const 3) #f)
                ,(make-PushImmediateOntoEnvironment (make-Const 4) #f)
                procedure-entry
                ;; doesn't matter about the procedure entry...
                ,(make-AssignPrimOp 
                          'proc
                          (make-MakeCompiledProcedure 'procedure-entry (make-ArityAtLeast 2) (list 0 1) 'procedure-entry))
                ,(make-AssignImmediate 'val (make-CompiledProcedureClosureReference (make-Reg 'proc) 1)))
              "M.v")
      "3")




