#lang whalesong (require "../selfhost-lang.rkt")

(require "arity-structs.rkt"
         "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "compiler-structs.rkt"
         "kernel-primitives.rkt"
         "optimize-il.rkt"
         "analyzer-structs.rkt"
         "../parameters.rkt"
         "../sets.rkt"
         "analyzer.rkt"
         ; racket/list
         ; racket/match
         )

; (require/typed "../logger.rkt" [log-debug (String -> Void)])

(require "compiler-helper.rkt")


; (require/typed "../parser/modprovide.rkt" [get-provided-names (Expression -> (Listof ModuleProvide))])
(require (only-in "../parser/modprovide.rkt" get-provided-names))


(provide (rename-out [-compile compile]
                     [compile raw-compile])
         compile-for-repl
         compile-general-procedure-call)


;; We keep track of which lambda is currently being compiled for potential optimizations
;; e.g. self tail calls.
(: current-lambda-being-compiled (Parameterof (U #f Lam)))
(define current-lambda-being-compiled (make-parameter #f))




(: -compile (Expression Target Linkage -> (Listof Statement)))
;; Generates the instruction-sequence stream.
;; Note: the toplevel generates the lambda body streams at the head, and then the
;; rest of the instruction stream.
(define (-compile exp target linkage)
  (define lambda-bodies (collect-all-lambdas-with-bodies exp))
  (define after-lam-bodies (make-label 'afterLamBodies))
  (define-values (before-pop-prompt-multiple before-pop-prompt)
    (new-linked-labels 'beforePopPrompt))
  (optimize-il
   (statements
    (append-instruction-sequences 
     
     ;; Layout the lambda bodies...
     (make-Goto (make-Label after-lam-bodies))
     (compile-lambda-bodies lambda-bodies)
     after-lam-bodies
     
     ;; Begin a prompted evaluation:
     (make-PushControlFrame/Prompt default-continuation-prompt-tag
                                   before-pop-prompt)
     (compile exp '() 'val return-linkage/nontail)
     before-pop-prompt-multiple
     (make-PopEnvironment (make-SubtractArg (make-Reg 'argcount) (make-Const 1))
                          (make-Const 0))
     before-pop-prompt
     (if (eq? target 'val)
         empty-instruction-sequence
         (make-AssignImmediate target (make-Reg 'val)))))))


;; Compiles an expression for the REPL.
;; The result of the repl evaluation will be a list in the var register.
(: compile-for-repl (Expression -> (Listof Statement)))
(define (compile-for-repl exp)
  (define lambda-bodies (collect-all-lambdas-with-bodies exp))
  (define after-lam-bodies: (make-label 'afterLamBodies))
  (define bundle-values-into-list: (make-label 'bundleValuesIntoList))
  (define abort-with-multiple-values: (make-label 'abortWithMultipleValues))
  (define last: (make-label 'last))
  (define-values (handle-multiple-return: handle-return:)
    (new-linked-labels 'afterPopPrompt))
  
  (optimize-il
   (statements
    (append-instruction-sequences
     ;; Layout the lambda bodies...
     (make-Goto (make-Label after-lam-bodies:))
     (compile-lambda-bodies lambda-bodies)
     
     after-lam-bodies:
     
     ;; Begin a prompted evaluation:
     (make-PushControlFrame/Prompt default-continuation-prompt-tag
                                   handle-return:)
     (compile exp '() 'val return-linkage/nontail)
     
     handle-multiple-return:
     ;; After coming back from the evaluation, rearrange the return
     ;; values, to call the continuation with those as arguments.
     (make-TestAndJump (make-TestZero (make-Reg 'argcount)) 
                       bundle-values-into-list:)
     handle-return:
     (make-PushImmediateOntoEnvironment (make-Reg 'val) #f)     
     bundle-values-into-list:
     (make-Goto (make-Label last:))
     
     last:
     ;; Finally, return to the success continuation on the stack.
     (make-AssignImmediate 'proc (make-ControlStackLabel))
     (make-PopControlFrame)
     (make-Goto (make-Reg 'proc))))))






(: end-with-linkage (Linkage CompileTimeEnvironment InstructionSequence -> InstructionSequence))
;; Add linkage for expressions.
(define (end-with-linkage linkage cenv instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-linkage cenv linkage)))




(: compile-linkage (CompileTimeEnvironment Linkage -> InstructionSequence))
;; Generates the code necessary to drive the rest of the computation (represented as the linkage).
(define (compile-linkage cenv linkage)
  (cond
    [(ReturnLinkage? linkage)
     (cond
       [(ReturnLinkage-tail? linkage)
        ;; Under tail calls, clear the environment of the current stack frame (represented by cenv)
        ;; and do the jump.
        (append-instruction-sequences
         (make-PopEnvironment (make-Const (length cenv)) 
                              (make-Const 0))
         (make-AssignImmediate 'proc (make-ControlStackLabel))
         (make-PopControlFrame)
         (make-Goto (make-Reg 'proc)))]
       [else
        ;; Under non-tail calls, leave the stack as is and just do the jump.
        (append-instruction-sequences
         (make-AssignImmediate 'proc (make-ControlStackLabel))
         (make-PopControlFrame)
         (make-Goto (make-Reg 'proc)))])]
    
    [(NextLinkage? linkage)
     empty-instruction-sequence]
    
    [(LabelLinkage? linkage)
     (make-Goto (make-Label (LabelLinkage-label linkage)))]))






(: compile (Expression CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; The main dispatching function for compilation.
;; Compiles an expression into an instruction sequence.
(define (compile exp cenv target linkage)
  (cond
    [(Top? exp)
     (compile-top exp cenv target linkage)]
    [(Module? exp)
     (compile-module exp cenv target linkage)]
    [(Constant? exp)
     (compile-constant exp cenv target linkage)]
    [(LocalRef? exp)
     (compile-local-reference exp cenv target linkage)]
    [(ToplevelRef? exp)
     (compile-toplevel-reference exp cenv target linkage)]
    [(ToplevelSet? exp)
     (compile-toplevel-set exp cenv target linkage)]
    [(Branch? exp)
     (compile-branch exp cenv target linkage)]
    [(Lam? exp)
     (compile-lambda exp cenv target linkage)]
    [(CaseLam? exp)
     (compile-case-lambda exp cenv target linkage)]
    [(EmptyClosureReference? exp)
     (compile-empty-closure-reference exp cenv target linkage)]
    [(Seq? exp)
     (compile-sequence (Seq-actions exp)
                       cenv
                       target
                       linkage)]
    [(Splice? exp)
     (compile-splice (Splice-actions exp)
                     cenv
                     target
                     linkage)]
    [(Begin0? exp)
     (compile-begin0 (Begin0-actions exp)
                     cenv
                     target
                     linkage)]
    [(App? exp)
     (compile-application exp cenv target linkage)]
    [(Let1? exp)
     (compile-let1 exp cenv target linkage)]
    [(LetVoid? exp)
     (compile-let-void exp cenv target linkage)]
    [(InstallValue? exp)
     (compile-install-value exp cenv target linkage)]
    [(BoxEnv? exp)
     (compile-box-environment-value exp cenv target linkage)]
    [(LetRec? exp)
     (compile-let-rec exp cenv target linkage)]
    [(WithContMark? exp)
     (compile-with-cont-mark exp cenv target linkage)]
    [(ApplyValues? exp)
     (compile-apply-values exp cenv target linkage)]
    [(DefValues? exp)
     (compile-def-values exp cenv target linkage)]
    [(PrimitiveKernelValue? exp)
     (compile-primitive-kernel-value exp cenv target linkage)]
    [(VariableReference? exp)
     (compile-variable-reference exp cenv target linkage)]
    [(Require? exp)
     (compile-require exp cenv target linkage)]))




(: compile-top (Top CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Generates code to write out the top prefix, evaluate the rest of the body,
;; and then pop the top prefix off afterwards.
(define (compile-top top cenv target linkage)
  (let* ([names (Prefix-names (Top-prefix top))]) ;:(Listof (U False Symbol GlobalBucket ModuleVariable))
    (end-with-linkage 
     linkage cenv
     (append-instruction-sequences
      (make-Perform (make-ExtendEnvironment/Prefix! names))
      (compile (Top-code top) 
               (cons (Top-prefix top) cenv)
               'val
               next-linkage/keep-multiple-on-stack)
      (make-AssignImmediate target (make-Reg 'val))
      (make-PopEnvironment (make-Const 1) 
                           (new-SubtractArg (make-Reg 'argcount)
                                            (make-Const 1)))))))





(: compile-module (Module CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Generates code to write out the top prefix, evaluate the rest of the body,
;; and then pop the top prefix off.
(define (compile-module mod cenv target linkage)
  (unless (Module? mod) (error 'compile-module "expected Module?"))
  (define name (Module-name mod))
  (define path (Module-path mod))
  (define prefix (Module-prefix mod))
  (define requires (Module-requires mod))
  (define provides (Module-provides mod))
  (define code (Module-code mod))
  ; (match mod [(struct Module (name path prefix requires provides code))
     (let* ([after-module-body (make-label 'afterModuleBody)]
            [module-entry (make-label 'module-entry)]
            [names (Prefix-names prefix)] ; : (Listof (U False Symbol GlobalBucket ModuleVariable))
            [module-cenv (list prefix)])  ; : CompileTimeEnvironment
       
       (end-with-linkage 
        linkage cenv
        (append-instruction-sequences
         (make-Perform (make-InstallModuleEntry! name path module-entry))
         (make-Goto (make-Label after-module-body))
         
         
         module-entry
         (make-Perform (make-MarkModuleInvoked! path))
         ;; Module body definition:
         ;; 1.  First invoke all the modules that this requires.
         (apply append-instruction-sequences
           (map compile-module-invoke (Module-requires mod)))
         
         ;; 2.  Store the prefix:
         (make-Perform (make-ExtendEnvironment/Prefix! names))
         (make-AssignImmediate (make-ModulePrefixTarget path)
                               (make-EnvWholePrefixReference 0))
         ;; 3.  Next, evaluate the module body.
         (compile (Module-code mod) 
                  (cons (Module-prefix mod) module-cenv)
                  'val
                  next-linkage/drop-multiple)
         
         ;; 4. Finally, cleanup and return.
         (make-PopEnvironment (make-Const 1) (make-Const 0))
         (make-AssignImmediate 'proc (make-ControlStackLabel))
         (make-PopControlFrame)
         
         ;; We sequester the prefix of the module with the record.
         (make-Perform (make-FinalizeModuleInvokation! path provides))
         (make-Goto (make-Reg 'proc))
         
         after-module-body))))

(: compile-require (Require CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-require exp cenv target linkage)
  (end-with-linkage linkage cenv
                    (append-instruction-sequences
                     (compile-module-invoke (Require-path exp))
                     (make-AssignImmediate target (make-Const (void))))))


(: compile-module-invoke (ModuleLocator -> InstructionSequence))
;; Generates code that will invoke a module (if it hasn't been invoked
;; yet) FIXME: assumes the module has already been loaded/linked.  We
;; should try to load the module, or error out if the module can't be
;; found.
(define (compile-module-invoke a-module-name)
  (cond
    [(kernel-module-name? a-module-name)
     empty-instruction-sequence]
    [else
     (define linked (make-label 'linked))
     (define-values (on-return-multiple on-return) (new-linked-labels 'onReturn))
     (append-instruction-sequences
      (make-TestAndJump (make-TestTrue (make-ModulePredicate a-module-name 'linked?))
                        linked)
      ;; TODO: try to link dynamically, using plt.runtime.currentModuleLoader.
      (make-Perform (make-LinkModule! a-module-name linked))
      ;; If that fails, finally raise an exception here that says that the module hasn't been
      ;; linked yet.
      ;(make-DebugPrint (make-Const 
      ;                    (format "DEBUG: the module ~a hasn't been linked in!!!"
      ;                            (ModuleLocator-name a-module-name))))
      ;(make-Goto (make-Label (LinkedLabel-label on-return)))
      linked
      (make-TestAndJump (make-TestTrue 
                         (make-ModulePredicate a-module-name 'invoked?))
                        (LinkedLabel-label on-return))
      (make-PushControlFrame/Call on-return)
      (make-Goto (ModuleEntry a-module-name))
      on-return-multiple
      (make-PopEnvironment (new-SubtractArg (make-Reg 'argcount)
                                            (make-Const 1))
                           (make-Const 0))
      on-return)]))






(: emit-singular-context (Linkage -> InstructionSequence))
;; Emits code specific to a construct that's guaranteed to produce a single value.
;;
;; This does two things:
;;
;; 1.  The emitted code raises a runtime error if the linkage requires 
;; multiple values will be produced, since there's no way to produce them.
;;
;; 2. In the case where the context is 'keep-multiple, it will also indicate a single
;; value by assigning to the argcount register.
(define (emit-singular-context linkage)
  (cond [(ReturnLinkage? linkage)
         ;; Callers who use ReturnLinkage are responsible for doing
         ;; runtime checks for the singular context.
         empty-instruction-sequence]
        [(or (NextLinkage? linkage)
             (LabelLinkage? linkage))
         (let ([context (linkage-context linkage)])
           (cond
             [(eq? context 'tail)
              empty-instruction-sequence]
             
             [(eq? context 'drop-multiple)
              empty-instruction-sequence]
             
             [(eq? context 'keep-multiple)
              (make-AssignImmediate 'argcount (make-Const 1))]
             
             [(natural? context)
              (if (= context 1)
                  empty-instruction-sequence
                  (append-instruction-sequences
                   (make-AssignImmediate 'argcount (make-Const 1))
                   (make-Perform (make-RaiseContextExpectedValuesError!
                                  context))))]))]))



(: compile-constant (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Generates output for constant values.
(define (compile-constant exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    ;; Compiles constant values.
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-AssignImmediate target (make-Const
                                                     (ensure-const-value (Constant-v exp))))
                       singular-context-check))))


(: compile-variable-reference (VariableReference CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-variable-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    ;; Compiles constant values.
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-AssignImmediate target exp)
                       singular-context-check))))


(: compile-local-reference (LocalRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles local variable references.
(define (compile-local-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-AssignImmediate target
                                             (make-EnvLexicalReference (LocalRef-depth exp)
                                                                       (LocalRef-unbox? exp)))
                       singular-context-check))))


(: compile-toplevel-reference (ToplevelRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles toplevel references.
(define (compile-toplevel-reference exp cenv target linkage)
  (define prefix (ensure-prefix (list-ref cenv (ToplevelRef-depth exp))))
  (define prefix-element (list-ref (Prefix-names prefix) (ToplevelRef-pos exp)))
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       
                       ;; If it's a module variable, we need to look there.
                       (cond
                         [(ModuleVariable? prefix-element)
                          (cond [(kernel-module-name? (ModuleVariable-module-name prefix-element))
                                 (make-AssignPrimOp target
                                                    (make-PrimitivesReference
                                                     (kernel-module-variable->primitive-name
                                                      prefix-element)))]
                                [else
                                 (make-AssignImmediate
                                  target
                                  (make-EnvPrefixReference (ToplevelRef-depth exp)
                                                           (ToplevelRef-pos exp)
                                                           #t))])]
                         [(GlobalBucket? prefix-element)
                          (append-instruction-sequences
                           (if (ToplevelRef-check-defined? exp)
                               (make-Perform (make-CheckGlobalBound! (GlobalBucket-name prefix-element)))
                               empty-instruction-sequence)
                           (make-AssignPrimOp
                            target
                            (make-GlobalsReference (GlobalBucket-name prefix-element))))]
                         [(or (eq? prefix-element #f) (symbol? prefix-element))
                          (append-instruction-sequences
                           (if (ToplevelRef-check-defined? exp)
                               (make-Perform (make-CheckToplevelBound!
                                              (ToplevelRef-depth exp)
                                              (ToplevelRef-pos exp)))
                               empty-instruction-sequence)
                           (if (ToplevelRef-constant? exp)
                               (make-Comment (format "Constant toplevel ref: ~s"
                                                     (extract-static-knowledge exp cenv)))
                               empty-instruction-sequence)
                           (make-AssignImmediate
                            target
                            (make-EnvPrefixReference (ToplevelRef-depth exp)
                                                     (ToplevelRef-pos exp)
                                                     #f)))])
                       singular-context-check))))






(: compile-toplevel-set (ToplevelSet CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a toplevel mutation.
(define (compile-toplevel-set exp cenv target linkage)
  (define prefix (ensure-prefix (list-ref cenv (ToplevelSet-depth exp))))
  (define prefix-element (list-ref (Prefix-names prefix) (ToplevelSet-pos exp)))
  
  (define top-target
    (cond
      [(ModuleVariable? prefix-element)
       (make-EnvPrefixReference (ToplevelSet-depth exp)
                                (ToplevelSet-pos exp)
                                #t)]
      [(GlobalBucket? prefix-element)
       (make-GlobalsReference (GlobalBucket-name prefix-element))]
      
      [(or (eq? prefix-element #f)
           (symbol? prefix-element))
       (make-EnvPrefixReference (ToplevelSet-depth exp)
                                (ToplevelSet-pos exp)
                                #f)]))
  (let ([get-value-code
         (cond
           ;; Special case: when set!-ing globals, see that they're defined first.
           [(GlobalBucket? prefix-element)
            (append-instruction-sequences
             (compile (ToplevelSet-value exp) cenv 'val next-linkage/expects-single)
             (make-Perform (make-CheckGlobalBound! (GlobalBucket-name prefix-element)))
             (make-AssignImmediate top-target (make-Reg 'val)))]
           [else
            (compile (ToplevelSet-value exp) cenv top-target next-linkage/expects-single)])]
        [singular-context-check (emit-singular-context linkage)])
    (end-with-linkage
     linkage
     cenv
     (append-instruction-sequences
      get-value-code
      (make-AssignImmediate target (make-Const (void)))
      singular-context-check))))


(: compile-branch (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a conditional branch.
(define (compile-branch exp cenv target linkage)
  (let ([f-branch: (make-label 'falseBranch)]  ; : Symbol 
        [after-if: (make-label 'afterIf)])     ; : Symbol
    (let ([consequent-linkage
           (cond
             [(NextLinkage? linkage)
              (let ([context (NextLinkage-context linkage)])
                (make-LabelLinkage after-if: context))]
             [(ReturnLinkage? linkage)
              linkage]
             [(LabelLinkage? linkage)
              linkage])])
      (let ([p-code (compile (Branch-predicate exp) cenv 'val next-linkage/expects-single)]
            [c-code (compile (Branch-consequent exp) cenv target consequent-linkage)]
            [a-code (compile (Branch-alternative exp) cenv target linkage)])
        (append-instruction-sequences 
         p-code
         (make-TestAndJump (make-TestFalse (make-Reg 'val))
                           f-branch:)
         c-code
         f-branch: a-code
         (if (NextLinkage? linkage)
             after-if:
             empty-instruction-sequence))))))


(: compile-sequence ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a sequence of expressions.  The last expression will be compiled in the provided linkage.
(define (compile-sequence seq cenv target linkage) 
  ;; All but the last will use next-linkage linkage.
  (cond [(empty? seq)
         (end-with-linkage linkage cenv empty-instruction-sequence)]
        [(empty? (rest seq))
         (compile (first seq) cenv target linkage)]
        [else
         (append-instruction-sequences 
          (compile (first seq) cenv 'val next-linkage/drop-multiple)
          (compile-sequence (rest seq) cenv target linkage))]))



(: compile-splice ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a sequence of expressions.  A continuation prompt wraps around each of the expressions
;; to delimit any continuation captures.
(define (compile-splice seq cenv target linkage)
  (cond [(empty? seq)
         (end-with-linkage linkage cenv empty-instruction-sequence)]
        [(empty? (rest seq))
         (define-values (on-return/multiple on-return)
           (new-linked-labels 'beforePromptPop))
         (end-with-linkage 
          linkage
          cenv
          (append-instruction-sequences 
           (make-PushControlFrame/Prompt default-continuation-prompt-tag
                                         on-return)
           (compile (first seq) cenv 'val return-linkage/nontail)
           (emit-values-context-check-on-procedure-return (linkage-context linkage)
                                                          on-return/multiple
                                                          on-return)
           (make-AssignImmediate target (make-Reg 'val))))]
        [else
         (define-values (on-return/multiple on-return)
           (new-linked-labels 'beforePromptPop))
         (append-instruction-sequences 
          (make-PushControlFrame/Prompt (make-DefaultContinuationPromptTag)
                                        on-return)
          
          (compile (first seq) cenv 'val return-linkage/nontail)
          on-return/multiple
          (make-PopEnvironment (new-SubtractArg (make-Reg 'argcount)
                                                (make-Const 1))
                               (make-Const 0))
          on-return
          (compile-splice (rest seq) cenv target linkage))]))


(: compile-begin0 ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-begin0 seq cenv target linkage)
  (cond
    [(empty? seq)
     (end-with-linkage linkage cenv empty-instruction-sequence)]
    [(empty? (rest seq))
     (compile (first seq) cenv target linkage)]
    [else
     (let ([evaluate-and-save-first-expression
            (let ([after-first-seq (make-label 'afterFirstSeqEvaluated)])
              (append-instruction-sequences
               ;; Evaluate the first expression in a multiple-value context, and get the values on the stack.
               (compile (first seq) cenv 'val next-linkage/keep-multiple-on-stack)
               
               (make-TestAndJump (make-TestZero (make-Reg 'argcount)) after-first-seq)
               (make-PushImmediateOntoEnvironment (make-Reg 'val) #f)
               after-first-seq
               ;; At this time, the argcount values are on the stack.
               ;; Next, we save those values temporarily in a throwaway control frame.
               (make-PushControlFrame/Generic)
               (make-AssignImmediate (make-ControlFrameTemporary 'pendingBegin0Count)
                                     (make-Reg 'argcount))
               (make-Perform (make-UnspliceRestFromStack! (make-Const 0) (make-Reg 'argcount)))
               (make-AssignImmediate (make-ControlFrameTemporary 'pendingBegin0Values)
                                     (make-EnvLexicalReference 0 #f))
               (make-PopEnvironment (make-Const 1) (make-Const 0))))]
           
           [reinstate-values-on-stack
            (let ([after-values-reinstated (make-label 'afterValuesReinstated)])
              (append-instruction-sequences
               ;; Reinstate the values of the first expression, and drop the throwaway control frame.
               (make-PushImmediateOntoEnvironment (make-ControlFrameTemporary 'pendingBegin0Values) #f)
               (make-Perform (make-SpliceListIntoStack! (make-Const 0)))
               (make-AssignImmediate 'argcount (make-ControlFrameTemporary 'pendingBegin0Count))
               (make-PopControlFrame)
               (make-TestAndJump (make-TestZero (make-Reg 'argcount)) after-values-reinstated)
               (make-AssignImmediate 'val (make-EnvLexicalReference 0 #f))
               (make-PopEnvironment (make-Const 1) (make-Const 0))
               after-values-reinstated))])
       
       (append-instruction-sequences
        evaluate-and-save-first-expression        
        
        (compile-sequence (rest seq) cenv 'val next-linkage/drop-multiple)
        
        reinstate-values-on-stack        
        
        (make-AssignImmediate target (make-Reg 'val))
        
        ;; TODO: context needs check for arguments.
        (cond
          [(ReturnLinkage? linkage)
           (cond
             [(ReturnLinkage-tail? linkage)
              (append-instruction-sequences 
               (make-PopEnvironment (make-Const (length cenv)) 
                                    (new-SubtractArg (make-Reg 'argcount)
                                                     (make-Const 1)))
               (make-AssignImmediate 'proc (make-ControlStackLabel/MultipleValueReturn))
               (make-PopControlFrame)
               (make-Goto (make-Reg 'proc)))]
             [else
              (append-instruction-sequences
               (make-AssignImmediate 'proc (make-ControlStackLabel/MultipleValueReturn))
               (make-PopControlFrame)
               (make-Goto (make-Reg 'proc)))])]
          
          [(NextLinkage? linkage)
           empty-instruction-sequence]
          
          [(LabelLinkage? linkage)
           (make-Goto (make-Label (LabelLinkage-label linkage)))])))]))




(: compile-lambda (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Write out code for lambda expressions.
;; The lambda will close over the free variables.
;; Assumption: all of the lambda bodies have already been written out at the top, in -compile.
(define (compile-lambda exp cenv target linkage) 
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage 
     linkage
     cenv
     (append-instruction-sequences
      (make-AssignPrimOp 
       target
       (make-MakeCompiledProcedure (Lam-entry-label exp)
                                   (Lam-arity exp)
                                   (Lam-closure-map exp)
                                   (Lam-name exp)))
      singular-context-check))))

(: compile-empty-closure-reference (EmptyClosureReference CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-empty-closure-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage
     linkage
     cenv
     (append-instruction-sequences      
      (make-AssignPrimOp 
       target
       (make-MakeCompiledProcedure (EmptyClosureReference-entry-label exp)
                                   (EmptyClosureReference-arity exp)
                                   empty
                                   (EmptyClosureReference-name exp)))
      singular-context-check))))




(: compile-case-lambda (CaseLam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Similar to compile-lambda.
(define (compile-case-lambda exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)]
        [n (length (CaseLam-clauses exp))])
    
    ;; We have to build all the lambda values, and then create a single CaseLam that holds onto
    ;; all of them.
    (end-with-linkage 
     linkage
     cenv
     (append-instruction-sequences
      ;; Make some temporary space for the lambdas
      
      (make-PushEnvironment n #f)
      
      ;; Compile each of the lambdas
      (apply append-instruction-sequences
        (map (lambda (lam target) #;([lam : (U Lam EmptyClosureReference)]
                                     [target : Target])
               (make-AssignPrimOp
                target
                (cond
                  [(Lam? lam)
                   (make-MakeCompiledProcedure (Lam-entry-label lam)
                                               (Lam-arity lam)
                                               (shift-closure-map (Lam-closure-map lam) n)
                                               (Lam-name lam))]
                  [(EmptyClosureReference? lam)
                   (make-MakeCompiledProcedure (EmptyClosureReference-entry-label lam)
                                               (EmptyClosureReference-arity lam)
                                               '()
                                               (EmptyClosureReference-name lam))])))
             (CaseLam-clauses exp)
             (build-list (length (CaseLam-clauses exp))
                         (lambda (i) ;  ([i : Natural])
                           (make-EnvLexicalReference i #f)))))
      
      ;; Make the case lambda as a regular compiled procedure.  Its closed values are the lambdas.
      (make-AssignPrimOp 
       (adjust-target-depth target n)
       (make-MakeCompiledProcedure (CaseLam-entry-label exp)
                                   (merge-arities (map Lam-arity (CaseLam-clauses exp)))
                                   (build-list n (lambda (i) #;([i : Natural]) i))
                                   (CaseLam-name exp)))
      
      ;; Finally, pop off the scratch space.
      (make-PopEnvironment (make-Const n) (make-Const 0))
      singular-context-check))))


(: Lam-arity ((U Lam EmptyClosureReference) -> Arity))
(define (Lam-arity lam)
  (cond
    [(Lam? lam)
     (if (Lam-rest? lam)
         (make-ArityAtLeast (Lam-num-parameters lam))
         (Lam-num-parameters lam))]
    [(EmptyClosureReference? lam)
     (if (EmptyClosureReference-rest? lam)
         (make-ArityAtLeast (EmptyClosureReference-num-parameters lam))
         (EmptyClosureReference-num-parameters lam))]))


(: EmptyClosureReference-arity (EmptyClosureReference -> Arity))
(define (EmptyClosureReference-arity lam)
  (if (EmptyClosureReference-rest? lam)
      (make-ArityAtLeast (EmptyClosureReference-num-parameters lam))
      (EmptyClosureReference-num-parameters lam)))




(: shift-closure-map ((Listof Natural) Natural -> (Listof Natural)))
(define (shift-closure-map closure-map n)
  (map (lambda (i) #;([i : Natural]) (+ i n))
       closure-map))


(: merge-arities ((Listof Arity) -> Arity))
(define (merge-arities arities)
  (cond [(empty? (rest arities))
         (first arities)]
        [else
         (let ([first-arity (first arities)]
               [merged-rest (merge-arities (rest arities))])
           (cond
             [(AtomicArity? first-arity)
              (cond [(AtomicArity? merged-rest)
                     (list first-arity merged-rest)]
                    [(listof-atomic-arity? merged-rest)
                     (cons first-arity merged-rest)])]
             [(listof-atomic-arity? first-arity)
              (cond [(AtomicArity? merged-rest)
                     (append first-arity (list merged-rest))]
                    [(listof-atomic-arity? merged-rest)
                     (append first-arity merged-rest)])]))]))



(: compile-lambda-shell (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Write out code for lambda expressions, minus the closure map.
;; Assumption: all of the lambda bodies have already been written out at the top, in -compile.
(define (compile-lambda-shell exp cenv target linkage) 
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage 
     linkage
     cenv
     (append-instruction-sequences
      (make-AssignPrimOp 
       target
       (make-MakeCompiledProcedureShell (Lam-entry-label exp)
                                        (if (Lam-rest? exp)
                                            (make-ArityAtLeast (Lam-num-parameters exp))
                                            (Lam-num-parameters exp))
                                        (Lam-name exp)))
      singular-context-check))))





(: compile-lambda-body (Lam CompileTimeEnvironment -> InstructionSequence))
;; Compiles the body of the lambda in the appropriate environment.
;; Closures will target their value to the 'val register, and use return linkage.
(define (compile-lambda-body exp cenv)
  (parameterize ([current-lambda-being-compiled exp])
    ;; (define all-applications (collect-lam-applications exp cenv))
    
    (let ([maybe-unsplice-rest-argument ; : InstructionSequence
           (if (Lam-rest? exp)
               (make-Perform 
                (make-UnspliceRestFromStack! 
                 (make-Const (Lam-num-parameters exp))
                 (new-SubtractArg (make-Reg 'argcount)
                                  (make-Const (Lam-num-parameters exp)))))
               empty-instruction-sequence)]
          [maybe-install-closure-values ; : InstructionSequence
           (if (not (empty? (Lam-closure-map exp)))
               (append-instruction-sequences
                (make-Perform (make-InstallClosureValues!
                               (length (Lam-closure-map exp)))))
               empty-instruction-sequence)]
          [lam-body-code ; : InstructionSequence
           (compile (Lam-body exp)
                    cenv
                    'val
                    return-linkage)])
      
      (append-instruction-sequences      
       (Lam-entry-label exp)
       (make-MarkEntryPoint (Lam-entry-label exp))
       (Comment (format "lambda body for ~a" (Lam-name exp)))
       maybe-unsplice-rest-argument
       maybe-install-closure-values
       lam-body-code))))


(: compile-case-lambda-body (CaseLam CompileTimeEnvironment -> InstructionSequence))
(define (compile-case-lambda-body exp cenv)
  (append-instruction-sequences
   
   (CaseLam-entry-label exp)
   
   (apply append-instruction-sequences
     (map (lambda (lam i) #;([lam : (U Lam EmptyClosureReference)]
                             [i : Natural])
            (let ([not-match (make-label 'notMatch)])
              (append-instruction-sequences
               (make-TestAndJump (make-TestClosureArityMismatch
                                  (make-CompiledProcedureClosureReference 
                                   (make-Reg 'proc) 
                                   i)
                                  (make-Reg 'argcount))
                                 not-match)
               ;; Set the procedure register to the lam
               (make-AssignImmediate 
                'proc 
                (make-CompiledProcedureClosureReference (make-Reg 'proc) i))
               
               (make-Goto (make-Label
                           (cond [(Lam? lam)
                                  (Lam-entry-label lam)]
                                 [(EmptyClosureReference? lam)
                                  (EmptyClosureReference-entry-label lam)])))
               
               not-match)))
          (CaseLam-clauses exp)
          (build-list (length (CaseLam-clauses exp)) (lambda (i) #;([i : Natural]) i))))))


(: compile-lambda-bodies ((Listof lam+cenv) -> InstructionSequence))
;; Compile several lambda bodies, back to back.
(define (compile-lambda-bodies exps)
  (cond
    [(empty? exps)
     empty-instruction-sequence]
    [else
     (let ([lam (lam+cenv-lam (first exps))]    ; : (U Lam CaseLam)
           [cenv (lam+cenv-cenv (first exps))]) ; : CompileTimeEnvironment
       (cond
         [(Lam? lam)
          (append-instruction-sequences (compile-lambda-body lam cenv)
                                        (compile-lambda-bodies (rest exps)))]
         [(CaseLam? lam)
          (append-instruction-sequences 
           (compile-case-lambda-body lam cenv)
           (compile-lambda-bodies (rest exps)))]))]))




(: extend-compile-time-environment/scratch-space (CompileTimeEnvironment Natural -> CompileTimeEnvironment))
(define (extend-compile-time-environment/scratch-space cenv n)
  (append (build-list n (lambda (i) #;([i : Natural])
                          '?))
          cenv))



(: compile-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles procedure application
;; Special cases: if we know something about the operator, the compiler will special case.
;; This includes:
;;     Known closure
;;     Known kernel primitive
;;  In the general case, we do general procedure application.
(define (compile-application exp cenv target linkage) 
  (let ([extended-cenv 
         (extend-compile-time-environment/scratch-space 
          cenv 
          (length (App-operands exp)))])
    
    (define (default)
      (compile-general-application exp cenv target linkage))
    
    (let ([op-knowledge ; : CompileTimeEnvironmentEntry
           (extract-static-knowledge (App-operator exp)
                                     extended-cenv)])
      (cond
        [(eq? op-knowledge '?)
         (default)]
        [(operator-is-statically-known-identifier? op-knowledge)
         => 
         (lambda (id)
           (cond
             [(KernelPrimitiveName/Inline? id)
              (compile-open-codeable-application id exp cenv target linkage)]
             [((current-primitive-identifier?) id)
              => (lambda (expected-arity)
                   (compile-primitive-application exp cenv target linkage id expected-arity))]
             [else
              (default)]))]
        [(StaticallyKnownLam? op-knowledge)
         (compile-statically-known-lam-application op-knowledge exp cenv target linkage)]
        [(Prefix? op-knowledge)
         (error 'impossible)]
        [(Const? op-knowledge)
         (append-instruction-sequences
          (make-AssignImmediate 'proc op-knowledge)
          (make-Perform
           (make-RaiseOperatorApplicationError! (make-Reg 'proc))))]
        [else
         (default)]))))


(: operator-is-statically-known-identifier? (CompileTimeEnvironmentEntry -> (U False Symbol)))
(define (operator-is-statically-known-identifier? op-knowledge)
  (cond [(PrimitiveKernelValue? op-knowledge)
         (let ([id (PrimitiveKernelValue-id op-knowledge)])
           id)]
        [(ModuleVariable? op-knowledge)
         (cond
           [(kernel-module-name? (ModuleVariable-module-name op-knowledge))
            (kernel-module-variable->primitive-name op-knowledge)]
           [else
            #f])]
        [else
         #f]))







(: compile-general-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-general-application exp cenv target linkage)
  (define n (length (App-operands exp)))
  (define extended-cenv (extend-compile-time-environment/scratch-space 
                         cenv 
                         (length (App-operands exp))))
  (define proc+operands-code
    (cond
      ;; Optimization: if the operand and operands are all side-effect-free, we don't need to
      ;; juggle.
      [(andmap side-effect-free-expression? (cons (App-operator exp) (App-operands exp)))
       (define proc-code (compile (App-operator exp) extended-cenv 'proc next-linkage/expects-single))
       (define operand-codes (map (lambda (operand target) #;([operand : Expression]
                                                              [target : Target])
                                    (compile operand
                                             extended-cenv
                                             target
                                             next-linkage/expects-single))
                                  (App-operands exp)
                                  (build-list (length (App-operands exp))
                                              (lambda (i) #;([i : Natural])
                                                (make-EnvLexicalReference i #f)))))
       (apply append-instruction-sequences proc-code operand-codes)]
      [else
       ;; Otherwise, we need to juggle a little.
       (define proc-code
         (compile (App-operator exp)
                  extended-cenv 
                  (if (empty? (App-operands exp))
                      'proc
                      (make-EnvLexicalReference 
                       (ensure-natural (sub1 (length (App-operands exp))))
                       #f))
                  next-linkage/expects-single))
       (define operand-codes
         (map (lambda (operand target) #;([operand : Expression]
                                          [target : Target])
                (compile operand
                         extended-cenv
                         target
                         next-linkage/expects-single))
              (App-operands exp)
              (build-list (length (App-operands exp))
                          (lambda (i) #;([i : Natural])
                            (if (< i (sub1 (length (App-operands exp))))
                                (make-EnvLexicalReference i #f)
                                'val)))))
       (append-instruction-sequences
        proc-code
        (juggle-operands operand-codes))]))
  
  (append-instruction-sequences
   (make-PushEnvironment (length (App-operands exp)) #f)
   proc+operands-code
   (make-AssignImmediate 'argcount (make-Const (length (App-operands exp))))
   (compile-general-procedure-call cenv
                                   (make-Const (length (App-operands exp)))
                                   target
                                   linkage)))






(: compile-primitive-application (App CompileTimeEnvironment Target Linkage Symbol Arity -> InstructionSequence))
(define (compile-primitive-application exp cenv target linkage primitive-name expected-arity)
  (let* ([extended-cenv
          (extend-compile-time-environment/scratch-space 
           cenv 
           (length (App-operands exp)))]
         [operand-codes (map (lambda (operand target) #;([operand : Expression]
                                                         [target : Target])
                               (compile operand
                                        extended-cenv
                                        target
                                        next-linkage/expects-single))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda (i) #;([i : Natural])
                                           (make-EnvLexicalReference i #f))))])
    (append-instruction-sequences
     (make-PushEnvironment (length (App-operands exp)) #f)
     (apply append-instruction-sequences operand-codes)
     
     ;; Optimization: if the expected arity is a known constant, we don't
     ;; need to touch argcount either.  If it's variable, we emit the argcount, since
     ;; it's something we need at runtime.
     (if (number? expected-arity)
         empty-instruction-sequence
         (make-AssignImmediate 'argcount (make-Const (length (App-operands exp)))))
     
     (if (arity-matches? expected-arity (length (App-operands exp)))
         (compile-primitive-procedure-call primitive-name
                                           cenv 
                                           (make-Const (length (App-operands exp)))
                                           target
                                           linkage)
         (append-instruction-sequences
          (compile (App-operator exp) extended-cenv 'proc next-linkage/expects-single)
          (make-Perform (make-RaiseArityMismatchError! 
                         (make-Reg 'proc)
                         expected-arity
                         (make-Const (length (App-operands exp))))))))))


;; If we know the procedure is implemented as a primitive (as opposed to a general closure),
;; we can do a little less work.
;; We don't need to check arity (as that's already been checked statically).
;; Assumes 1. the procedure value is NOT loaded into proc.  We know statically what the
;;            procedure is supposed to be.
;;         2. (OPTIONAL) number-of-arguments has been conditionally written into the argcount register,
; ;        3. the number-of-arguments values are on the stack.
(: compile-primitive-procedure-call (Symbol CompileTimeEnvironment OpArg Target Linkage -> InstructionSequence))
(define (compile-primitive-procedure-call primitive-name cenv number-of-arguments target linkage)
  (end-with-linkage
   linkage
   cenv
   (append-instruction-sequences
    (make-AssignPrimOp 'val (make-ApplyPrimitiveProcedure primitive-name))
    (make-PopEnvironment number-of-arguments (make-Const 0))
    (if (eq? target 'val)
        empty-instruction-sequence
        (make-AssignImmediate target (make-Reg 'val)))
    (emit-singular-context linkage))))






(: compile-open-codeable-application
   (KernelPrimitiveName/Inline App CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; This is a special case of application, where the operator is statically
;; known to be in the set of hardcoded primitives, and where we can open-code
;; the application.
;;
;; There's a special case optimization we can perform: we can avoid touching
;; the stack for constant arguments; rather than allocate (length (App-operands exp))
;; stack slots, we can do less than that.
;;
;; We have to be sensitive to mutation.
(define (compile-open-codeable-application kernel-op exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)]
        [n (length (App-operands exp))])
    
    (define expected-operand-types
      (kernel-primitive-expected-operand-types kernel-op n))
    
    (: make-runtime-arity-mismatch-code (Arity -> InstructionSequence))
    (define (make-runtime-arity-mismatch-code expected-arity)
      ;; We compile the code to generate a runtime arity error here.
      (end-with-linkage
       linkage cenv
       (append-instruction-sequences
        (make-PushEnvironment n #f)
        (apply append-instruction-sequences
          (map (lambda (operand target) #;([operand : Expression]
                                           [target : Target])
                 (compile operand
                          (extend-compile-time-environment/scratch-space 
                           cenv 
                           (length (App-operands exp)))
                          target
                          next-linkage/expects-single))
               (App-operands exp)
               (build-list (length (App-operands exp))
                           (lambda (i) #;([i : Natural])
                             (make-EnvLexicalReference i #f)))))
        (make-AssignImmediate 'proc (make-PrimitiveKernelValue kernel-op))
        (make-AssignImmediate 'argcount
                              (make-Const (length (App-operands exp))))
        (make-Perform (make-RaiseArityMismatchError! 
                       (make-Reg 'proc)
                       expected-arity
                       (make-Const n))))))
    
    (cond
      [(IncorrectArity? expected-operand-types)
       (make-runtime-arity-mismatch-code (IncorrectArity-expected expected-operand-types))]
      
      [(not (= n (length expected-operand-types)))
       (make-runtime-arity-mismatch-code (length expected-operand-types))]
      
      [else
       (cond
         ;; If all the arguments are primitive enough (all constants, localrefs, or toplevelrefs),
         ;; then application requires no stack space at all, and application is especially side-effect-free.
         [(andmap side-effect-free-expression? (App-operands exp))
          (let* ([operand-knowledge
                  (map (lambda (arg) #;([arg : Expression])
                         (extract-static-knowledge 
                          arg 
                          (extend-compile-time-environment/scratch-space 
                           cenv n)))
                       (App-operands exp))]
                 
                 [typechecks?
                  (map (lambda (dom known) #;([dom : OperandDomain]
                                              [known : CompileTimeEnvironmentEntry])
                         (not (redundant-check? dom known)))
                       expected-operand-types
                       operand-knowledge)]
                 
                 [operand-poss
                  (side-effect-free-operands->opargs (map (lambda (op)  #;([op : Expression])
                                                            (ensure-side-effect-free-expression
                                                             (adjust-expression-depth op n n)))
                                                          (App-operands exp))
                                                     operand-knowledge)])
            (end-with-linkage
             linkage cenv
             (append-instruction-sequences
              (make-AssignPrimOp target
                                 (make-CallKernelPrimitiveProcedure 
                                  kernel-op 
                                  operand-poss
                                  expected-operand-types
                                  typechecks?))
              singular-context-check)))]
         
         [else
          ;; Otherwise, we can split the operands into two categories: constants, and the rest.
          (let*-values ([(constant-operands rest-operands)
                         (split-operands-by-constants 
                          (App-operands exp))]
                        
                        ;; here, we rewrite the stack references so they assume no scratch space
                        ;; used by the constant operands.
                        [(extended-cenv constant-operands rest-operands)
                         (values (extend-compile-time-environment/scratch-space 
                                  cenv 
                                  (length rest-operands))
                                 
                                 (map (lambda (constant-operand) #;([constant-operand : Expression])
                                        (ensure-side-effect-free-expression
                                         (adjust-expression-depth constant-operand
                                                                  (length constant-operands)
                                                                  n)))
                                      constant-operands)
                                 
                                 (map (lambda (rest-operand) #;([rest-operand : Expression])
                                        (adjust-expression-depth rest-operand
                                                                 (length constant-operands)
                                                                 n))
                                      rest-operands))]
                        [(constant-operand-knowledge)
                         (map (lambda (arg) #;([arg : Expression])
                                (extract-static-knowledge arg extended-cenv))
                              constant-operands)]
                        
                        [(operand-knowledge)
                         (append constant-operand-knowledge
                                 (map (lambda (arg) #;([arg : Expression])
                                        (extract-static-knowledge arg extended-cenv))
                                      rest-operands))]
                        
                        [(typechecks?)
                         (map (lambda (dom known) #;([dom : OperandDomain]
                                                     [known : CompileTimeEnvironmentEntry])
                                (not (redundant-check? dom known)))
                              expected-operand-types
                              operand-knowledge)]
                        
                        [(stack-pushing-code) 
                         (make-PushEnvironment (length rest-operands)
                                               #f)]
                        [(stack-popping-code) 
                         (make-PopEnvironment (make-Const (length rest-operands))
                                              (make-Const 0))]
                        
                        [(constant-operand-poss)
                         (side-effect-free-operands->opargs constant-operands constant-operand-knowledge)]
                        
                        [(rest-operand-poss)
                         (build-list (length rest-operands)
                                     (lambda (i) #;([i : Natural])
                                       (make-EnvLexicalReference i #f)))]
                        [(rest-operand-code)
                         (apply append-instruction-sequences
                           (map (lambda (operand target) #;([operand : Expression]
                                                            [target : Target])
                                  (compile operand 
                                           extended-cenv 
                                           target 
                                           next-linkage/expects-single))
                                rest-operands
                                rest-operand-poss))])
            
            (end-with-linkage
             linkage cenv
             (append-instruction-sequences
              stack-pushing-code
              rest-operand-code
              (make-AssignPrimOp (adjust-target-depth target (length rest-operands))
                                 (make-CallKernelPrimitiveProcedure 
                                  kernel-op 
                                  (append constant-operand-poss rest-operand-poss)
                                  expected-operand-types
                                  typechecks?))
              stack-popping-code
              singular-context-check)))])])))




(: ensure-side-effect-free-expression (Expression -> (U Constant ToplevelRef LocalRef PrimitiveKernelValue)))
(define (ensure-side-effect-free-expression e)
  (if (or (Constant? e)
          (LocalRef? e)
          (ToplevelRef? e)
          (PrimitiveKernelValue? e))
      e
      (error 'ensure-side-effect-free-expression)))


(: side-effect-free-expression? (Expression -> Boolean))
;; Produces true if the expression is side-effect-free and constant.
;; TODO: generalize this so that it checks that the expression is
;; side-effect free.  If it's side-effect free, then we can compute
;; the expressions in any order.
(define (side-effect-free-expression? e)
  (or (Constant? e)
      (LocalRef? e)
      (ToplevelRef? e)
      (PrimitiveKernelValue? e)))


(: side-effect-free-operands->opargs ((Listof (U Constant LocalRef ToplevelRef PrimitiveKernelValue))
                                      (Listof CompileTimeEnvironmentEntry)
                                      -> (Listof OpArg)))
;; Produces a list of OpArgs if all the operands are particularly side-effect-free.
(define (side-effect-free-operands->opargs rands knowledge)
  (map (lambda (e k) #;([e : (U Constant LocalRef ToplevelRef PrimitiveKernelValue)]
                        [k : CompileTimeEnvironmentEntry])
         (cond
           [(Constant? e)
            (make-Const (ensure-const-value (Constant-v e)))]
           [(LocalRef? e)
            (make-EnvLexicalReference (LocalRef-depth e)
                                      (LocalRef-unbox? e))]
           [(ToplevelRef? e)
            (cond
              [(ModuleVariable? k)
               (cond [(kernel-module-name? (ModuleVariable-module-name k))
                      (make-PrimitiveKernelValue
                       (kernel-module-variable->primitive-name k))]
                     [else
                      (make-EnvPrefixReference (ToplevelRef-depth e)
                                               (ToplevelRef-pos e)
                                               #t)])]
              [else
               (make-EnvPrefixReference (ToplevelRef-depth e) (ToplevelRef-pos e) #f)])]
           [(PrimitiveKernelValue? e)
            e]))
       rands
       knowledge))



(: redundant-check? (OperandDomain CompileTimeEnvironmentEntry -> Boolean))
;; Produces true if we know the knowledge implies the domain-type.
(define (redundant-check? domain-type knowledge)
  (cond
    [(eq? domain-type 'any)
     #t]
    [else
     (cond [(Const? knowledge)
            (case domain-type
              [(number)
               (number? (Const-const knowledge))]
              [(string)
               (string? (Const-const knowledge))]
              [(box)
               (box? (Const-const knowledge))]
              [(list)
               (list? (Const-const knowledge))]
              [(vector)
               (vector? (Const-const knowledge))]
              [(pair)
               (pair? (Const-const knowledge))]
              [(caarpair)
               (let ([x (Const-const knowledge)])
                 (and (pair? x)
                      (pair? (car x))))])]
           [else
            #f])]))


(: split-operands-by-constants 
   ((Listof Expression)  -> 
                         (values (Listof (U Constant))
                                 (Listof Expression))))
;; Splits off the list of operations into two: a prefix of
;; constant expressions, and the remainder.  TODO: if we can
;; statically determine what arguments are immutable, regardless of
;; side effects, we can do a much better job here...
(define (split-operands-by-constants rands)
  (let loop ; : (values (Listof (U Constant)) (Listof Expression))
    ([rands rands] ; : (Listof Expression) 
     [constants ; : (Listof (U Constant))
      empty])
    (cond [(empty? rands)
           (values (reverse constants) empty)]
          [else (let ([e (first rands)])
                  (if (or (Constant? e)
                          ;; These two are commented out because it's not sound otherwise.
                          #;(and (LocalRef? e) (not (LocalRef-unbox? e))) 
                          #;(and (ToplevelRef? e)
                                 (let ([prefix (ensure-prefix 
                                                (list-ref cenv (ToplevelRef-depth e)))])
                                   (ModuleVariable? 
                                    (list-ref prefix (ToplevelRef-pos e))))))
                      (loop (rest rands) (cons e constants))
                      (values (reverse constants) rands)))])))


; (define-predicate natural? Natural)

; (define-predicate atomic-arity-list? (Listof (U Natural ArityAtLeast)))
(define (atomic-arity-list? o)
  (and (list? o) (andmap (λ (o) (or (natural? o) (ArityAtLeast? o))))))

(: arity-matches? (Arity Natural -> Boolean))
(define (arity-matches? an-arity n)
  (cond
    [(natural? an-arity)
     (= an-arity n)]
    [(ArityAtLeast? an-arity)
     (>= n (ArityAtLeast-value an-arity))]
    [(atomic-arity-list? an-arity)
     (ormap (lambda (an-arity) #;([an-arity : (U Natural ArityAtLeast)])
              (cond
                [(natural? an-arity)
                 (= an-arity n)]
                [(ArityAtLeast? an-arity)
                 (>= n (ArityAtLeast-value an-arity))]))
            an-arity)]))



(: compile-statically-known-lam-application 
   (StaticallyKnownLam App CompileTimeEnvironment Target Linkage 
                       -> InstructionSequence))
(define (compile-statically-known-lam-application static-operator-knowledge exp cenv target linkage)
  (let ([arity-check
         (cond [(arity-matches? (StaticallyKnownLam-arity static-operator-knowledge)
                                (length (App-operands exp)))
                empty-instruction-sequence]
               [else
                (make-Perform
                 (make-RaiseArityMismatchError! 
                  (make-Reg 'proc)
                  (StaticallyKnownLam-arity static-operator-knowledge)
                  (make-Const (length (App-operands exp)))))])])
    (let* ([extended-cenv 
            (extend-compile-time-environment/scratch-space 
             cenv 
             (length (App-operands exp)))]
           [operand-codes (map (lambda (operand target) #;([operand : Expression]
                                                           [target : Target])
                                 (compile operand 
                                          extended-cenv 
                                          target 
                                          next-linkage/expects-single))
                               (App-operands exp)
                               (build-list (length (App-operands exp))
                                           (lambda (i) #;([i : Natural])
                                             (make-EnvLexicalReference i #f))))]
           [proc-code (compile (App-operator exp)
                               extended-cenv 
                               'proc
                               next-linkage/expects-single)])    
      (append-instruction-sequences
       (make-PushEnvironment (length (App-operands exp)) #f)
       (apply append-instruction-sequences operand-codes)
       proc-code
       arity-check
       (compile-procedure-call/statically-known-lam static-operator-knowledge 
                                                    cenv
                                                    extended-cenv 
                                                    (length (App-operands exp))
                                                    target
                                                    linkage)))))


(: juggle-operands ((Listof InstructionSequence) -> InstructionSequence))
;; Installs the operators.  At the end of this,
;; the procedure lives in 'proc, and the operands on the environment stack.
(define (juggle-operands operand-codes)
  (let loop ; : InstructionSequence 
    ([ops  operand-codes]) ; : (Listof InstructionSequence)
    (cond
      ;; If there are no operands, no need to juggle.
      [(null? ops)
       empty-instruction-sequence]
      [(null? (rest ops))
       (let ([n (ensure-natural (sub1 (length operand-codes)))]) ; : Natural 
         ;; The last operand needs to be handled specially: it currently lives in 
         ;; val.  We move the procedure at env[n] over to proc, and move the
         ;; last operand at 'val into env[n].
         (append-instruction-sequences 
          (car ops)
          (make-AssignImmediate 'proc 
                                (make-EnvLexicalReference n #f))
          (make-AssignImmediate (make-EnvLexicalReference n #f)
                                (make-Reg 'val))))]
      [else
       ;; Otherwise, add instructions to juggle the operator and operands in the stack.
       (append-instruction-sequences (car ops)
                                     (loop (rest ops)))])))


(: linkage-context (Linkage -> ValuesContext))
(define (linkage-context linkage)
  (cond
    [(ReturnLinkage? linkage)
     (cond [(ReturnLinkage-tail? linkage)
            'tail]
           [else
            'keep-multiple])]
    [(NextLinkage? linkage)
     (NextLinkage-context linkage)]
    [(LabelLinkage? linkage)
     (LabelLinkage-context linkage)]))



(: compile-general-procedure-call (CompileTimeEnvironment OpArg Target Linkage 
                                                          ->
                                                          InstructionSequence))
;; Assumes the following:
;; 1.  the procedure value has been loaded into the proc register.
;; 2.  the n values passed in has been written into argcount register.
;; 3.  environment stack contains the n operand values.
;;
;; n is the number of arguments passed in.
;; cenv is the compile-time enviroment before arguments have been shifted in.
;; extended-cenv is the compile-time environment after arguments have been shifted in.
(define (compile-general-procedure-call cenv number-of-arguments target linkage)
  (end-with-linkage
   linkage
   cenv
   (append-instruction-sequences
    (make-Perform (make-CheckClosureAndArity!))
    (compile-compiled-procedure-application cenv
                                            number-of-arguments
                                            'dynamic
                                            target
                                            linkage))))





(: compile-procedure-call/statically-known-lam 
   (StaticallyKnownLam CompileTimeEnvironment CompileTimeEnvironment Natural Target Linkage -> InstructionSequence))
(define (compile-procedure-call/statically-known-lam static-operator-knowledge cenv extended-cenv n target linkage)
  (let* ([after-call  (make-label 'afterCall)] ; : Symbol
         [compiled-linkage  (if (and (ReturnLinkage? linkage)
                                     (ReturnLinkage-tail? linkage))
                                linkage
                                (make-LabelLinkage 
                                 after-call
                                 (linkage-context linkage)))]) ; : Linkage
    (append-instruction-sequences
     (make-AssignImmediate 'argcount
                           (make-Const n))
     (compile-compiled-procedure-application cenv
                                             (make-Const n)
                                             (make-Label 
                                              (StaticallyKnownLam-entry-point static-operator-knowledge))
                                             target
                                             compiled-linkage)
     (end-with-linkage
      linkage
      cenv
      after-call))))






(: compile-compiled-procedure-application (CompileTimeEnvironment OpArg (U Label 'dynamic) Target Linkage -> InstructionSequence))
;; This is the heart of compiled procedure application.  A lot of things happen here.
;;
;;     Procedure linkage.
;;     Handling of multiple-value-returns.
;;     Tail calls.
;;
;; Three fundamental cases for general compiled-procedure application.
;;    1.  Tail calls.
;;    2.  Non-tail calls (next/label linkage) that write to val
;;    3.  Calls in argument position (next/label linkage) that write to the stack.
(define (compile-compiled-procedure-application cenv number-of-arguments entry-point target linkage)
  (define entry-point-target
    ;; Optimization: if the entry-point is known to be a static label,
    ;; use that.  Otherwise, grab the entry point from the proc register.
    (cond [(Label? entry-point)
           entry-point]
          [(eq? entry-point 'dynamic)
           (make-CompiledProcedureEntry (make-Reg 'proc))]))
  
  ;; If the target isn't val, migrate the value from val into it.
  (define maybe-migrate-val-to-target
    (cond
      [(eq? target 'val)
       empty-instruction-sequence]
      [else
       (make-AssignImmediate target (make-Reg 'val))]))
  
  (define-values (on-return/multiple on-return) (new-linked-labels 'procReturn))
  
  ;; This code does the initial jump into the procedure.  Clients of this code
  ;; are expected to generate the proc-return-multiple and proc-return code afterwards.
  (define nontail-jump-into-procedure
    (append-instruction-sequences 
     (make-PushControlFrame/Call on-return)
     (make-Goto entry-point-target)))
  
  (cond [(ReturnLinkage? linkage)
         (cond
           [(eq? target 'val)
            (cond
              [(ReturnLinkage-tail? linkage)
               ;; This case happens when we're in tail position.
               ;; We clean up the stack right before the jump, and do not add
               ;; to the control stack.
               (let ([reuse-the-stack
                      (make-PopEnvironment (make-Const (length cenv))
                                           number-of-arguments)])
                 (append-instruction-sequences
                  reuse-the-stack
                  ;; Assign the proc value of the existing call frame.
                  (make-Perform (make-SetFrameCallee! (make-Reg 'proc)))
                  (make-Goto entry-point-target)))]
              
              [else	    
               ;; This case happens when we should be returning to a caller, but where
               ;; we are not in tail position.
               (make-Goto entry-point-target)])]
           
           [else
            (error 'compile "return linkage, target not val: ~s" target)])]
        
        
        [(or (NextLinkage? linkage) (LabelLinkage? linkage))
         (let* ([context (linkage-context linkage)]
                
                [check-values-context-on-procedure-return
                 (emit-values-context-check-on-procedure-return context on-return/multiple on-return)]
                
                [maybe-jump-to-label
                 (if (LabelLinkage? linkage)
                     (make-Goto (make-Label (LabelLinkage-label linkage)))
                     empty-instruction-sequence)])
           
           (append-instruction-sequences
            nontail-jump-into-procedure
            check-values-context-on-procedure-return
            maybe-migrate-val-to-target
            maybe-jump-to-label))]))



(: emit-values-context-check-on-procedure-return (ValuesContext Symbol LinkedLabel -> InstructionSequence))
;; When we come back from a procedure call, the following code ensures the context's expectations
;; are met.
(define (emit-values-context-check-on-procedure-return context on-return/multiple on-return)
  (cond                                          
    [(eq? context 'tail)
     (append-instruction-sequences on-return/multiple
                                   on-return)]
    
    [(eq? context 'drop-multiple)
     (append-instruction-sequences
      on-return/multiple
      (make-PopEnvironment (new-SubtractArg (make-Reg 'argcount) (make-Const 1))
                           (make-Const 0))
      on-return)]
    
    [(eq? context 'keep-multiple)
     (let ([after-return (make-label 'afterReturn)])
       (append-instruction-sequences
        on-return/multiple
        (make-Goto (make-Label after-return))
        on-return
        (make-AssignImmediate 'argcount (make-Const 1))
        after-return))]
    
    [(natural? context)
     (cond
       [(= context 1)
        (append-instruction-sequences
         on-return/multiple
         (make-Perform
          (make-RaiseContextExpectedValuesError! 1))
         on-return)]
       [else
        (let ([after-value-check (make-label 'afterValueCheck)])
          (append-instruction-sequences
           on-return/multiple
           ;; if the wrong number of arguments come in, die
           (make-TestAndJump (make-TestZero (new-SubtractArg (make-Reg 'argcount)
                                                             (make-Const context)))
                             after-value-check)
           on-return
           (make-Perform
            (make-RaiseContextExpectedValuesError! context))
           after-value-check))])]))






(: compile-let1 (Let1 CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Single value binding.  Since there's only one rhs, we have more static guarantees we can make,
;; which is why we can use extract-static-knowledge here.
(define (compile-let1 exp cenv target linkage)
  (let* ([rhs-code ; : InstructionSequence 
          (compile (Let1-rhs exp)
                   (cons '? cenv)
                   (make-EnvLexicalReference 0 #f)
                   next-linkage/expects-single)]
         [after-body-code ; : Symbol 
          (make-label 'afterLetBody)]
         [extended-cenv ; : CompileTimeEnvironment 
          (cons (extract-static-knowledge (Let1-rhs exp)
                                          (cons '? cenv))
                cenv)]
         [context ; : ValuesContext 
          (linkage-context linkage)]
         [let-linkage ; : Linkage
          (cond
            [(NextLinkage? linkage)
             linkage]
            [(ReturnLinkage? linkage)
             (cond [(ReturnLinkage-tail? linkage)
                    linkage]
                   [else
                    (make-LabelLinkage after-body-code (linkage-context linkage))])]
            [(LabelLinkage? linkage)
             (make-LabelLinkage after-body-code (LabelLinkage-context linkage))])]
         
         [body-target ; : Target 
          (adjust-target-depth target 1)]
         [body-code ; : InstructionSequence
          (compile (Let1-body exp) extended-cenv body-target let-linkage)])
    (end-with-linkage 
     linkage
     extended-cenv
     (append-instruction-sequences
      (make-PushEnvironment 1 #f)
      rhs-code
      body-code
      after-body-code
      
      
      ;; We want to clear out the scratch space introduced by the
      ;; let1.  However, there may be multiple values coming
      ;; back at this point, from the evaluation of the body.  We
      ;; look at the context and route around those values
      ;; appropriate.
      (cond
        [(eq? context 'tail)
         empty-instruction-sequence]
        [(eq? context 'drop-multiple)
         (make-PopEnvironment (make-Const 1)
                              (make-Const 0))]
        [(eq? context 'keep-multiple)
         ;; dynamic number of arguments that need
         ;; to be preserved
         
         (make-PopEnvironment (make-Const 1)
                              (new-SubtractArg
                               (make-Reg 'argcount)
                               (make-Const 1)))]
        [else
         (cond [(= context 0)
                (make-PopEnvironment (make-Const 1)                                  
                                     (make-Const 0))]
               [(= context 1)
                (make-PopEnvironment (make-Const 1)
                                     (make-Const 0))]
               [else
                ;; n-1 values on stack that we need to route
                ;; around
                (make-PopEnvironment (make-Const 1)
                                     (new-SubtractArg
                                      (make-Const context)
                                      (make-Const 1)))])])))))




(: compile-let-void (LetVoid CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Binding several values.  Unlike before, it has less knowledge about what values will be bound,
;; and so there's less analysis here.
(define (compile-let-void exp cenv target linkage)
  (let* ([n  (LetVoid-count exp)] ; : Natural
         [after-let  (make-label 'afterLet)] ; : Symbol
         [after-body-code  (make-label 'afterLetBody)] ; : Symbol
         [extended-cenv ; : CompileTimeEnvironment
          (append (build-list (LetVoid-count exp) 
                              (lambda (i) #;([i : Natural]) '?))
                  cenv)]
         [context  (linkage-context linkage)] ; : ValuesContext
         [let-linkage ; : Linkage
          (cond
            [(NextLinkage? linkage)
             linkage]
            [(ReturnLinkage? linkage)
             (cond
               [(ReturnLinkage-tail? linkage)
                linkage]
               [else
                (make-LabelLinkage after-body-code context)])]
            [(LabelLinkage? linkage)
             (make-LabelLinkage after-body-code context)])]
         [body-target ; : Target 
          (adjust-target-depth target n)]
         [body-code ; : InstructionSequence
          (compile (LetVoid-body exp) extended-cenv body-target let-linkage)])
    (end-with-linkage 
     linkage
     extended-cenv
     (append-instruction-sequences 
      (make-PushEnvironment n (LetVoid-boxes? exp))
      body-code
      after-body-code
      
      ;; We want to clear out the scratch space introduced by the
      ;; let-void.  However, there may be multiple values coming
      ;; back at this point, from the evaluation of the body.  We
      ;; look at the context and route around those values
      ;; appropriate.
      (cond
        [(eq? context 'tail)
         empty-instruction-sequence]
        [(eq? context 'drop-multiple)
         (make-PopEnvironment (make-Const n)
                              (make-Const 0))]
        [(eq? context 'keep-multiple)
         ;; dynamic number of arguments that need
         ;; to be preserved
         (make-PopEnvironment (make-Const n)
                              (new-SubtractArg
                               (make-Reg 'argcount)
                               (make-Const 1)))]
        [else
         (cond [(= context 0)
                (make-PopEnvironment (make-Const n)                                  
                                     (make-Const 0))]
               [(= context 1)
                (make-PopEnvironment (make-Const n)
                                     (make-Const 0))]
               [else
                
                ;; n-1 values on stack that we need to route
                ;; around
                (make-PopEnvironment (make-Const n)
                                     (new-SubtractArg
                                      (make-Const context)
                                      (make-Const 1)))])])
      after-let))))



(: compile-let-rec (LetRec CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiled recursive Lams.  Each lambda is installed as a shell, and then the closures
;; are installed in-place.
(define (compile-let-rec exp cenv target linkage)
  (let* ([n (length (LetRec-procs exp))] ; : Natural 
         [extended-cenv ; : CompileTimeEnvironment
          (append (map (lambda (p) #;([p : Lam])
                         (extract-static-knowledge 
                          p
                          (append (build-list (length (LetRec-procs exp))
                                              (lambda (i) #;([i : Natural])
                                                '?))
                                  (drop cenv n))))
                       (LetRec-procs exp))
                  (drop cenv n))]
         [n (length (LetRec-procs exp))] ; : Natural 
         [after-body-code (make-label 'afterBodyCode)] ; : Symbol
         [letrec-linkage (cond    ;  : Linkage 
                           [(NextLinkage? linkage)
                            linkage]
                           [(ReturnLinkage? linkage)
                            (cond
                              [(ReturnLinkage-tail? linkage)
                               linkage]
                              [else
                               (make-LabelLinkage after-body-code
                                                  (linkage-context linkage))])]
                           [(LabelLinkage? linkage)
                            (make-LabelLinkage after-body-code
                                               (LabelLinkage-context linkage))])])
    (end-with-linkage
     linkage
     extended-cenv
     (append-instruction-sequences
      
      ;; Install each of the closure shells.
      (apply append-instruction-sequences
        (map (lambda (lam i) #;([lam : Lam]
                                [i : Natural])
               (compile-lambda-shell lam 
                                     extended-cenv
                                     (make-EnvLexicalReference i #f) 
                                     next-linkage/expects-single))
             (LetRec-procs exp)
             (build-list n (lambda (i) #;([i : Natural]) i))))
      
      ;; Fix the closure maps of each
      (apply append-instruction-sequences
        (map (lambda (lam i) #;([lam : Lam]
                                [i : Natural])
               (append-instruction-sequences
                (make-Perform (make-FixClosureShellMap! i 
                                                        (Lam-closure-map lam)))))
             (LetRec-procs exp)
             (build-list n (lambda (i) #;([i : Natural]) i))))
      
      ;; Compile the body
      (compile (LetRec-body exp) extended-cenv target letrec-linkage)
      
      after-body-code))))



(: compile-install-value (InstallValue CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-install-value exp cenv target linkage)
  (append-instruction-sequences
   (let ([count (InstallValue-count exp)])
     (cond [(= count 0)
            (end-with-linkage
             linkage
             cenv
             (append-instruction-sequences
              (compile (InstallValue-body exp)
                       cenv
                       target
                       (make-NextLinkage 0))
              (make-AssignImmediate target (make-Const (void)))
              (emit-singular-context linkage)))]
           [(= count 1)
            (append-instruction-sequences
             (end-with-linkage
              linkage
              cenv
              (append-instruction-sequences
               (compile (InstallValue-body exp)
                        cenv
                        (make-EnvLexicalReference (InstallValue-depth exp) (InstallValue-box? exp))
                        (make-NextLinkage 1))
               (make-AssignImmediate target (make-Const (void)))
               (emit-singular-context linkage))))]
           [else
            (end-with-linkage
             linkage
             cenv
             (append-instruction-sequences
              (compile (InstallValue-body exp)
                       cenv
                       'val
                       (make-NextLinkage count))
              (apply append-instruction-sequences
                (map (lambda (to from) #;([to : EnvLexicalReference]
                                          [from : OpArg])
                       (append-instruction-sequences
                        (make-AssignImmediate to from)))
                     (build-list count (lambda (i) #;([i : Natural])
                                         (make-EnvLexicalReference (+ i 
                                                                      (InstallValue-depth exp)
                                                                      (ensure-natural (sub1 count)))
                                                                   (InstallValue-box? exp))))
                     (cons (make-Reg 'val) 
                           (build-list (sub1 count) (lambda (i) #;([i : Natural])
                                                      (make-EnvLexicalReference i #f))))))
              (make-PopEnvironment (make-Const (sub1 count)) (make-Const 0))
              (make-AssignImmediate target (make-Const (void)))
              (emit-singular-context linkage)))]))))



(: compile-box-environment-value (BoxEnv CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-box-environment-value exp cenv target linkage)
  (append-instruction-sequences
   (make-AssignPrimOp (make-EnvLexicalReference (BoxEnv-depth exp) #f)
                      (make-MakeBoxedEnvironmentValue (BoxEnv-depth exp)))
   (compile (BoxEnv-body exp) cenv target linkage)))




(: compile-with-cont-mark (WithContMark CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-with-cont-mark exp cenv target linkage)
  
  (: in-return-context (-> InstructionSequence))
  (define (in-return-context)
    (append-instruction-sequences
     (compile (WithContMark-key exp) cenv 'val next-linkage/expects-single)
     (make-AssignImmediate
      (make-ControlFrameTemporary 'pendingContinuationMarkKey)
      (make-Reg 'val))
     (compile (WithContMark-value exp) cenv 'val next-linkage/expects-single)
     (make-Perform (make-InstallContinuationMarkEntry!))
     (compile (WithContMark-body exp) cenv target linkage)))
  
  (: in-other-context ((U NextLinkage LabelLinkage) -> InstructionSequence))
  (define (in-other-context linkage)
    (define-values (on-return/multiple: on-return:)
      (new-linked-labels 'procReturn))
    (define context (linkage-context linkage))
    (define check-values-context-on-procedure-return
      (emit-values-context-check-on-procedure-return
       context on-return/multiple: on-return:))
    (define maybe-migrate-val-to-target
      (cond
        [(eq? target 'val)
         empty-instruction-sequence]
        [else
         (make-AssignImmediate target (make-Reg 'val))]))
    (append-instruction-sequences
     (make-PushControlFrame/Call on-return:)
     (compile (WithContMark-key exp) cenv 'val next-linkage/expects-single)
     (make-AssignImmediate (make-ControlFrameTemporary 'pendingContinuationMarkKey)
                           (make-Reg 'val))
     (compile (WithContMark-value exp) cenv 'val next-linkage/expects-single)
     (make-Perform (make-InstallContinuationMarkEntry!))
     (compile (WithContMark-body exp) cenv 'val return-linkage/nontail)
     check-values-context-on-procedure-return
     maybe-migrate-val-to-target))
  (cond
    [(ReturnLinkage? linkage)
     (in-return-context)]
    [(NextLinkage? linkage) 
     (in-other-context linkage)]
    [(LabelLinkage? linkage)
     (append-instruction-sequences
      (in-other-context linkage)
      (make-Goto (make-Label (LabelLinkage-label linkage))))]))


(: compile-apply-values (ApplyValues CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-apply-values exp cenv target linkage)
  ;(log-debug (format "apply values ~a" exp))
  (let ([on-zero (make-label 'onZero)]
        [after-args-evaluated (make-label 'afterArgsEvaluated)]
        [consumer-info
         (extract-static-knowledge (ApplyValues-proc exp) cenv)])
    (append-instruction-sequences
     
     ;; Save the procedure value temporarily in a control stack frame
     (make-PushControlFrame/Generic)
     (compile (ApplyValues-proc exp) 
              cenv 
              (make-ControlFrameTemporary 'pendingApplyValuesProc)
              next-linkage/expects-single)
     
     ;; Then evaluate the value producer in a context that expects
     ;; the return values to be placed onto the stack.
     (compile (ApplyValues-args-expr exp)
              cenv 
              'val
              next-linkage/keep-multiple-on-stack)
     
     (make-TestAndJump (make-TestZero (make-Reg 'argcount)) after-args-evaluated)
     ;; In the common case where we do get values back, we push val onto the stack too,
     ;; so that we have n values on the stack before we jump to the procedure call.
     (make-PushImmediateOntoEnvironment (make-Reg 'val) #f)
     
     after-args-evaluated
     ;; Retrieve the procedure off the temporary control frame.
     (make-AssignImmediate 
      'proc 
      (make-ControlFrameTemporary 'pendingApplyValuesProc))
     
     ;; Pop off the temporary control frame
     (make-PopControlFrame)
     
     
     ;; Finally, do the generic call into the consumer function.
     ;; FIXME: we have more static knowledge here of what the operator is.
     ;; We can make this faster.
     (compile-general-procedure-call cenv (make-Reg 'argcount) target linkage))))


(: compile-def-values (DefValues CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-def-values exp cenv target linkage)
  (let* ([ids (DefValues-ids exp)]
         [rhs (DefValues-rhs exp)]
         [n (length ids)])
    ;; First, compile the body, which will produce right side values.
    (end-with-linkage 
     linkage 
     cenv
     (append-instruction-sequences
      (compile rhs cenv 'val (make-NextLinkage (length ids)))
      
      ;; Now install each of the values in place.  The first value's in val, and the rest of the
      ;; values are on the stack.
      (if (> n 0)
          (apply append-instruction-sequences
            (map (lambda (id from) #;([id : ToplevelRef]
                                      [from : OpArg])
                   (define prefix 
                     (ensure-prefix (list-ref cenv (ToplevelRef-depth id))))
                   (define prefix-element (list-ref (Prefix-names prefix) (ToplevelRef-pos id)))  
                   (cond
                     [(GlobalBucket? prefix-element)
                      (make-AssignImmediate (make-GlobalsReference (GlobalBucket-name prefix-element))
                                            from)]
                     [else
                      ;; Slightly subtle: the toplevelrefs were with respect to the
                      ;; stack at the beginning of def-values, but at the moment,
                      ;; there may be additional values that are currently there.
                      (make-AssignImmediate
                       (make-EnvPrefixReference (+ (ensure-natural (sub1 n))
                                                   (ToplevelRef-depth id))
                                                (ToplevelRef-pos id)
                                                #f)
                       from)]))
                 ids
                 (if (> n 0) 
                     (cons (make-Reg 'val)
                           (build-list (sub1 n)
                                       (lambda (i) #;([i : Natural])
                                         (make-EnvLexicalReference i #f))))
                     empty)))
          empty-instruction-sequence)
      
      ;; Make sure any multiple values are off the stack.
      (if (> (length ids) 1)
          (make-PopEnvironment (make-Const (sub1 (length ids)))
                               (make-Const 0))
          empty-instruction-sequence)
      
      ;; Finally, set the target to void.
      
      (make-AssignImmediate target (make-Const (void)))
      (emit-singular-context linkage)))))



(: compile-primitive-kernel-value (PrimitiveKernelValue CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-primitive-kernel-value exp cenv target linkage)
  (let ([id (PrimitiveKernelValue-id exp)])
    (cond
      [(KernelPrimitiveName? id)   
       (let ([singular-context-check (emit-singular-context linkage)])
         ;; Compiles constant values.
         (end-with-linkage linkage
                           cenv
                           (append-instruction-sequences
                            (make-AssignImmediate target exp)
                            singular-context-check)))]
      [else
       ;; Maybe warn about the unimplemented kernel primitive.
       (unless (set-contains? (current-seen-unimplemented-kernel-primitives)
                              id)
         (set-insert! (current-seen-unimplemented-kernel-primitives)
                      id)
         ((current-warn-unimplemented-kernel-primitive) id))
       
       (make-Perform (make-RaiseUnimplementedPrimitiveError! id))])))







(: ensure-natural (Integer -> Natural))
(define (ensure-natural n)
  (if (>= n 0)
      n
      (error 'ensure-natural "Not a natural: ~s\n" n)))


(: ensure-lam (Any -> Lam))
(define (ensure-lam x)
  (if (Lam? x)
      x
      (error 'ensure-lam "Not a Lam: ~s" x)))


(: ensure-toplevelref (Any -> ToplevelRef))
(define (ensure-toplevelref x)
  (if (ToplevelRef? x)
      x
      (error 'ensure-toplevelref "Not a ToplevelRef: ~s" x)))


(: adjust-target-depth (Target Natural -> Target))
(define (adjust-target-depth target n)
  (cond
    [(eq? target 'val)
     target]
    [(eq? target 'proc)
     target]
    [(eq? target 'argcount)
     target]
    [(EnvLexicalReference? target)
     (make-EnvLexicalReference (+ n (EnvLexicalReference-depth target))
                               (EnvLexicalReference-unbox? target))]
    [(EnvPrefixReference? target)
     (make-EnvPrefixReference (+ n (EnvPrefixReference-depth target))
                              (EnvPrefixReference-pos target)
                              (EnvPrefixReference-modvar? target))]
    [(PrimitivesReference? target)
     target]
    [(GlobalsReference? target)
     target]
    [(ControlFrameTemporary? target)
     target]
    [(ModulePrefixTarget? target)
     target]
    [(ModuleVariable? target)
     target]))



(: adjust-expression-depth (Expression Natural Natural -> Expression))
;; Redirects references to the stack to route around a region of size n.
;; The region begins at offset skip into the environment.
(define (adjust-expression-depth exp n skip)
  (cond
    [(Top? exp)
     (make-Top (Top-prefix exp)
               (adjust-expression-depth (Top-code exp) n (add1 skip)))]
    
    [(Module? exp)
     (make-Module (Module-name exp)
                  (Module-path exp)
                  (Module-prefix exp)
                  (Module-requires exp)
                  (Module-provides exp)
                  (adjust-expression-depth (Module-code exp) n (add1 skip)))]
    
    [(Constant? exp)
     exp]
    
    [(ToplevelRef? exp)
     (if (< (ToplevelRef-depth exp) skip)
         exp
         (make-ToplevelRef (ensure-natural (- (ToplevelRef-depth exp) n))
                           (ToplevelRef-pos exp)
                           (ToplevelRef-constant? exp)
                           (ToplevelRef-check-defined? exp)))]
    
    [(LocalRef? exp)
     (if (< (LocalRef-depth exp) skip)
         exp
         (make-LocalRef (ensure-natural (- (LocalRef-depth exp) n))
                        (LocalRef-unbox? exp)))]
    
    [(ToplevelSet? exp)
     (if (< (ToplevelSet-depth exp) skip)
         (make-ToplevelSet (ToplevelSet-depth exp)
                           (ToplevelSet-pos exp)
                           (adjust-expression-depth (ToplevelSet-value exp) n skip))
         (make-ToplevelSet (ensure-natural (- (ToplevelSet-depth exp) n))
                           (ToplevelSet-pos exp)
                           (adjust-expression-depth (ToplevelSet-value exp) n skip)))]
    
    [(Branch? exp)
     (make-Branch (adjust-expression-depth (Branch-predicate exp) n skip)
                  (adjust-expression-depth (Branch-consequent exp) n skip)
                  (adjust-expression-depth (Branch-alternative exp) n skip))]
    
    [(Lam? exp)
     (make-Lam (Lam-name exp)
               (Lam-num-parameters exp)
               (Lam-rest? exp)
               (Lam-body exp)
               (map (lambda (d) #;([d : Natural]) 
                      (if (< d skip)
                          d
                          (ensure-natural (- d n))))
                    (Lam-closure-map exp))
               (Lam-entry-label exp))]
    
    [(CaseLam? exp)
     (make-CaseLam (CaseLam-name exp)
                   (map (lambda (lam) #;([lam : (U Lam EmptyClosureReference)])
                          (cond
                            [(Lam? lam)
                             (ensure-lam (adjust-expression-depth lam n skip))]
                            [(EmptyClosureReference? lam)
                             lam]))
                        (CaseLam-clauses exp))
                   (CaseLam-entry-label exp))]
    
    [(EmptyClosureReference? exp)
     exp]
    
    [(Seq? exp)
     (make-Seq (map (lambda (action) #;([action : Expression])
                      (adjust-expression-depth action n skip))
                    (Seq-actions exp)))]
    
    [(Splice? exp)
     (make-Splice (map (lambda (action) #;([action : Expression])
                         (adjust-expression-depth action n skip))
                       (Splice-actions exp)))]
    
    [(Begin0? exp)
     (make-Begin0 (map (lambda (action) #;([action : Expression])
                         (adjust-expression-depth action n skip))
                       (Begin0-actions exp)))]
    
    [(App? exp)
     (make-App (adjust-expression-depth (App-operator exp) n 
                                        (+ skip (length (App-operands exp))))
               (map (lambda (operand) #; ([operand : Expression])
                      (adjust-expression-depth 
                       operand n (+ skip (length (App-operands exp)))))
                    (App-operands exp)))]
    
    [(Let1? exp)
     (make-Let1 (adjust-expression-depth (Let1-rhs exp) n (add1 skip))
                (adjust-expression-depth (Let1-body exp) n (add1 skip)))]
    
    [(LetVoid? exp)
     (make-LetVoid (LetVoid-count exp)
                   (adjust-expression-depth (LetVoid-body exp) 
                                            n
                                            (+ skip (LetVoid-count exp)))
                   (LetVoid-boxes? exp))]
    
    [(LetRec? exp)
     (make-LetRec (let loop ; : (Listof Lam) 
                    ([procs (LetRec-procs exp)]) ; : (Listof Lam) 
                    (cond
                      [(empty? procs)
                       '()]
                      [else
                       (cons (ensure-lam (adjust-expression-depth 
                                          (first procs)
                                          n 
                                          skip))
                             (loop (rest procs)))]))
                  (adjust-expression-depth (LetRec-body exp) n 
                                           skip))]
    
    [(InstallValue? exp)
     (if (< (InstallValue-depth exp) skip)
         (make-InstallValue (InstallValue-count exp)
                            (InstallValue-depth exp)
                            (adjust-expression-depth (InstallValue-body exp)
                                                     n
                                                     skip)
                            (InstallValue-box? exp))
         (make-InstallValue (InstallValue-count exp)
                            (ensure-natural (- (InstallValue-depth exp) n))
                            (adjust-expression-depth (InstallValue-body exp)
                                                     n
                                                     skip)
                            (InstallValue-box? exp)))]
    
    [(BoxEnv? exp)
     (if (< (BoxEnv-depth exp) skip)
         (make-BoxEnv (BoxEnv-depth exp)
                      (adjust-expression-depth (BoxEnv-body exp) n skip))
         (make-BoxEnv (ensure-natural (- (BoxEnv-depth exp) n))
                      (adjust-expression-depth (BoxEnv-body exp) n skip)))]
    
    [(WithContMark? exp)
     (make-WithContMark (adjust-expression-depth (WithContMark-key exp) n skip)
                        (adjust-expression-depth (WithContMark-value exp) n skip)
                        (adjust-expression-depth (WithContMark-body exp) n skip))]
    [(ApplyValues? exp)
     (make-ApplyValues (adjust-expression-depth (ApplyValues-proc exp) n skip)
                       (adjust-expression-depth (ApplyValues-args-expr exp) n skip))]
    
    [(DefValues? exp)
     (make-DefValues (map (lambda (id) #;([id : ToplevelRef])
                            (ensure-toplevelref
                             (adjust-expression-depth id n skip)))
                          (DefValues-ids exp))
                     (adjust-expression-depth (DefValues-rhs exp) n skip))]
    
    [(PrimitiveKernelValue? exp)
     exp]
    
    [(VariableReference? exp)
     (make-VariableReference 
      (ensure-toplevelref
       (adjust-expression-depth (VariableReference-toplevel exp) n skip)))]
    [(Require? exp)
     exp]))
