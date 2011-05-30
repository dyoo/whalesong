#lang typed/racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "compiler-structs.rkt"
         "kernel-primitives.rkt"
         "optimize-il.rkt"
	 "analyzer-structs.rkt"
         "analyzer.rkt"
         "../parameters.rkt"
         "../sets.rkt"
         racket/match
         racket/bool
         racket/list)

(provide (rename-out [-compile compile])
         compile-general-procedure-call
         append-instruction-sequences)



(: current-analysis (Parameterof Analysis))
(define current-analysis (make-parameter (empty-analysis)))



(: -compile (Expression Target Linkage -> (Listof Statement)))
;; Generates the instruction-sequence stream.
;; Note: the toplevel generates the lambda body streams at the head, and then the
;; rest of the instruction stream.
(define (-compile exp target linkage)
  (parameterize ([current-analysis (analyze exp)])
    (let* ([after-lam-bodies (make-label 'afterLamBodies)]
           [before-pop-prompt-multiple (make-label 'beforePopPromptMultiple)]
           [before-pop-prompt (make-LinkedLabel
                               (make-label 'beforePopPrompt)
                               before-pop-prompt-multiple)])
      (optimize-il
       (statements
        (append-instruction-sequences 
         
         ;; Layout the lambda bodies...
         (make-instruction-sequence 
          `(,(make-GotoStatement (make-Label after-lam-bodies))))
         (compile-lambda-bodies (collect-all-lambdas-with-bodies exp))
         after-lam-bodies
         
         ;; Begin a prompted evaluation:
         (make-instruction-sequence
          `(,(make-PushControlFrame/Prompt default-continuation-prompt-tag
                                           before-pop-prompt)))
         (compile exp '() 'val return-linkage/nontail)
         before-pop-prompt-multiple
         (make-instruction-sequence
          `(,(make-PopEnvironment (make-Reg 'argcount) (make-Const 0))))
         before-pop-prompt
         (if (eq? target 'val)
             empty-instruction-sequence
             (make-instruction-sequence
              `(,(make-AssignImmediateStatement target (make-Reg 'val)))))))))))

(define-struct: lam+cenv ([lam : (U Lam CaseLam)]
                          [cenv : CompileTimeEnvironment]))



(: collect-all-lambdas-with-bodies (Expression -> (Listof lam+cenv)))
;; Finds all the lambdas in the expression.
(define (collect-all-lambdas-with-bodies exp)
  (let: loop : (Listof lam+cenv)
        ([exp : Expression exp]
         [cenv : CompileTimeEnvironment '()])
        
        (cond
          [(Top? exp)
           (loop (Top-code exp) (cons (Top-prefix exp) cenv))]
          [(Module? exp)
           (loop (Module-code exp) (cons (Module-prefix exp) cenv))]
          [(Constant? exp)
           '()]
          [(LocalRef? exp)
           '()]
          [(ToplevelRef? exp)
           '()]
          [(ToplevelSet? exp)
           (loop (ToplevelSet-value exp) cenv)]
          [(Branch? exp)
           (append (loop (Branch-predicate exp) cenv)
                   (loop (Branch-consequent exp) cenv)
                   (loop (Branch-alternative exp) cenv))]
          [(Lam? exp)
           (cons (make-lam+cenv exp cenv)
                 (loop (Lam-body exp) 
                       (extract-lambda-cenv exp cenv)))]
          [(CaseLam? exp)
           (cons (make-lam+cenv exp cenv)
                 (apply append (map (lambda: ([lam : (U Lam EmptyClosureReference)])
                                             (loop lam cenv))
                                    (CaseLam-clauses exp))))]
          
          [(EmptyClosureReference? exp)
           '()]
          
          [(Seq? exp)
           (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                              (Seq-actions exp)))]
          [(Splice? exp)
           (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                              (Splice-actions exp)))]
          [(Begin0? exp)
           (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                              (Begin0-actions exp)))]
          [(App? exp)
           (let ([new-cenv (append (build-list (length (App-operands exp)) (lambda: ([i : Natural]) '?))
                                   cenv)])
             (append (loop (App-operator exp) new-cenv)
                     (apply append (map (lambda: ([e : Expression]) (loop e new-cenv)) (App-operands exp)))))]
          [(Let1? exp)
           (append (loop (Let1-rhs exp)
                         (cons '? cenv))
                   (loop (Let1-body exp) 
                         (cons (extract-static-knowledge (Let1-rhs exp) (cons '? cenv)) 
                               cenv)))]
          [(LetVoid? exp)
           (loop (LetVoid-body exp) 
                 (append (build-list (LetVoid-count exp) (lambda: ([i : Natural]) '?))
                         cenv))]
          [(InstallValue? exp)
           (loop (InstallValue-body exp) cenv)]
          [(BoxEnv? exp)
           (loop (BoxEnv-body exp) cenv)]
          [(LetRec? exp)
           (let ([n (length (LetRec-procs exp))])
             (let ([new-cenv (append (map (lambda: ([p : Lam]) 
                                                   (extract-static-knowledge 
                                                    p 
                                                    (append (build-list (length (LetRec-procs exp))
                                                                        (lambda: ([i : Natural]) '?))
                                                            (drop cenv n))))
                                          (LetRec-procs exp))
                                     (drop cenv n))])
               (append (apply append 
                              (map (lambda: ([lam : Lam])
                                            (loop lam new-cenv))
                                   (LetRec-procs exp)))
                       (loop (LetRec-body exp) new-cenv))))]
          [(WithContMark? exp)
           (append (loop (WithContMark-key exp) cenv)
                   (loop (WithContMark-value exp) cenv)
                   (loop (WithContMark-body exp) cenv))]
          [(ApplyValues? exp)
           (append (loop (ApplyValues-proc exp) cenv)
                   (loop (ApplyValues-args-expr exp) cenv))]
          [(DefValues? exp)
           (append (loop (DefValues-rhs exp) cenv))]
          [(PrimitiveKernelValue? exp)
           '()]
          [(VariableReference? exp)
           (loop (VariableReference-toplevel exp) cenv)]
          [(Require? exp)
           '()])))



(: extract-lambda-cenv (Lam CompileTimeEnvironment -> CompileTimeEnvironment))
;; Given a Lam and the ambient environment, produces the compile time environment for the
;; body of the lambda.
(define (extract-lambda-cenv lam cenv)
  (append (map (lambda: ([d : Natural])
                        (list-ref cenv d))
               (Lam-closure-map lam))
          (build-list (if (Lam-rest? lam)
                          (add1 (Lam-num-parameters lam))
                          (Lam-num-parameters lam))
                      (lambda: ([i : Natural]) '?))))




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
        (make-instruction-sequence 
         `(,(make-PopEnvironment (make-Const (length cenv)) 
                                 (make-Const 0))
           ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel))
           ,(make-PopControlFrame)
           ,(make-GotoStatement (make-Reg 'proc))))]
       [else
        ;; Under non-tail calls, leave the stack as is and just do the jump.
        (make-instruction-sequence
         `(,(make-AssignImmediateStatement 'proc (make-ControlStackLabel))
           ,(make-PopControlFrame)
           ,(make-GotoStatement (make-Reg 'proc))))])]
    
    [(NextLinkage? linkage)
     empty-instruction-sequence]
 
    [(LabelLinkage? linkage)
     (make-instruction-sequence 
      `(,(make-GotoStatement (make-Label (LabelLinkage-label linkage)))))]))






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
;; and then pop the top prefix off.
(define (compile-top top cenv target linkage)
  (let*: ([names : (Listof (U False Symbol GlobalBucket ModuleVariable)) (Prefix-names (Top-prefix top))])
         (end-with-linkage 
          linkage cenv
          (append-instruction-sequences
           (make-instruction-sequence 
            `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! names))))
           (compile (Top-code top) 
                    (cons (Top-prefix top) cenv)
                    'val
                    next-linkage/drop-multiple)
           (make-instruction-sequence
            `(,(make-AssignImmediateStatement target (make-Reg 'val))
	      ,(make-PopEnvironment (make-Const 1) 
                                    (make-Const 0))))))))





(: compile-module (Module CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Generates code to write out the top prefix, evaluate the rest of the body,
;; and then pop the top prefix off.
(define (compile-module mod cenv target linkage)
  ;; fixme: this is not right yet.  This should instead install a module record
  ;; that has not yet been invoked.
  ;; fixme: This also needs to generate code for the requires and provides.
  (match mod
    [(struct Module (name path prefix requires code))
     (let*: ([after-module-body (make-label 'afterModuleBody)]
             [module-entry (make-label 'module-entry)]
             [names : (Listof (U False Symbol GlobalBucket ModuleVariable))
                    (Prefix-names prefix)]
             [module-cenv : CompileTimeEnvironment (list prefix)])
       
       (end-with-linkage 
        linkage cenv
        (append-instruction-sequences
         (make-PerformStatement (make-InstallModuleEntry! name path module-entry))
         (make-GotoStatement (make-Label after-module-body))

         
         module-entry
         (make-PerformStatement (make-MarkModuleInvoked! path))
         ;; Module body definition:
         ;; 1.  First invoke all the modules that this requires.
         #;(make-DebugPrint (make-Const "handling internal requires"))
         (apply append-instruction-sequences
                (map compile-module-invoke (Module-requires mod)))
         
         ;; 2.  Next, evaluate the module body.
         #;(make-DebugPrint (make-Const (format "evaluating module body of ~s" path)))
         (make-PerformStatement (make-ExtendEnvironment/Prefix! names))

         (make-AssignImmediateStatement (make-ModulePrefixTarget path)
                                        (make-EnvWholePrefixReference 0))
         ;; TODO: we need to sequester the prefix of the module with the record.
         (compile (Module-code mod) 
                  (cons (Module-prefix mod) module-cenv)
                  'val
                  next-linkage/drop-multiple)

         #;(make-DebugPrint (make-Const (format "About to clean up ~s" path)))
         
         ;; 3. Finally, cleanup and return.
         (make-PopEnvironment (make-Const 1) (make-Const 0))
         (make-AssignImmediateStatement 'proc (make-ControlStackLabel))
         (make-PopControlFrame)
         #;(make-DebugPrint (make-Const "Returning from module invokation."))
         #;(make-DebugPrint (make-Reg 'proc))
         
         (make-PerformStatement (make-FinalizeModuleInvokation! path))
         (make-GotoStatement (make-Reg 'proc))
         
         after-module-body)))]))

(: compile-require (Require CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-require exp cenv target linkage)
  (end-with-linkage linkage cenv
   (append-instruction-sequences
    (compile-module-invoke (Require-path exp))
    (make-instruction-sequence
     `(,(make-AssignImmediateStatement target (make-Const (void))))))))


(: compile-module-invoke (ModuleLocator -> InstructionSequence))
;; Generates code that will invoke a module (if it hasn't been invoked yet)
;; FIXME: assumes the module has already been linked.  We should error out
;; if the module hasn't been linked yet.
(define (compile-module-invoke a-module-name)
  (cond
   [(kernel-module-name? a-module-name)
    empty-instruction-sequence]
   [else
    (let* ([linked (make-label 'linked)]
           [already-loaded (make-label 'alreadyLoaded)]
           [on-return-multiple (make-label 'onReturnMultiple)]
           [on-return (make-LinkedLabel (make-label 'onReturn)
                                        on-return-multiple)])
      (make-instruction-sequence
       `(,(make-TestAndBranchStatement (make-TestTrue
                                        (make-IsModuleLinked a-module-name))
                                       linked)
         ;; TODO: raise an exception here that says that the module hasn't been
         ;; linked yet.
         ,(make-DebugPrint (make-Const 
                            (format "DEBUG: the module ~a hasn't been linked in!!!"
                                    (ModuleLocator-name a-module-name))))
         ,(make-GotoStatement (make-Label already-loaded))
         ,linked
         ,(make-TestAndBranchStatement (make-TestTrue 
                                        (make-IsModuleInvoked a-module-name))
                                       already-loaded)
         #;,(make-DebugPrint (make-Const (format "entering module ~s" a-module-name)))
         ,(make-PushControlFrame/Call on-return)
         ,(make-GotoStatement (ModuleEntry a-module-name))
         ,on-return-multiple
         ,(make-PopEnvironment (make-SubtractArg (make-Reg 'argcount)
                                                 (make-Const 1))
                               (make-Const 0))
         ,on-return
         #;,(make-DebugPrint (make-Const (format "coming back from module ~s" a-module-name)))
         ,already-loaded)))]))


(: kernel-module-name? (ModuleLocator -> Boolean))
;; Produces true if the module is hardcoded.
(define (kernel-module-name? name)
  ((current-kernel-module-locator?) name))




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
              (make-instruction-sequence
               `(,(make-AssignImmediateStatement 'argcount (make-Const 1))))]

             [(natural? context)
              (if (= context 1)
                  empty-instruction-sequence
                  (make-instruction-sequence
                   `(,(make-AssignImmediateStatement 'argcount
                                                     (make-Const 1))
                     ,(make-PerformStatement
                       (make-RaiseContextExpectedValuesError!
                        context)))))]))]))



(: compile-constant (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Generates output for constant values.
(define (compile-constant exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    ;; Compiles constant values.
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-instruction-sequence
                        `(,(make-AssignImmediateStatement target (make-Const (Constant-v exp)))))
                       singular-context-check))))


(: compile-variable-reference (VariableReference CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-variable-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    ;; Compiles constant values.
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-instruction-sequence
                        `(,(make-AssignImmediateStatement target exp)))
                       singular-context-check))))


(: compile-local-reference (LocalRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles local variable references.
(define (compile-local-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-instruction-sequence
                        `(,(make-AssignImmediateStatement 
                            target
                            (make-EnvLexicalReference (LocalRef-depth exp)
                                                      (LocalRef-unbox? exp)))))
                       singular-context-check))))


(: compile-toplevel-reference (ToplevelRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles toplevel references.
(define (compile-toplevel-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage linkage
                      cenv
                      (append-instruction-sequences
                       (make-instruction-sequence
                        `(,(make-Comment (format "Checking the prefix of length ~s" 
                                                 (length (Prefix-names (ensure-prefix (list-ref cenv (ToplevelRef-depth exp)))))))
                          ,(make-PerformStatement (make-CheckToplevelBound!
                                                   (ToplevelRef-depth exp)
                                                   (ToplevelRef-pos exp)))
                          ,(make-AssignImmediateStatement 
                            target
                            (make-EnvPrefixReference (ToplevelRef-depth exp)
                                                     (ToplevelRef-pos exp)))))
                       singular-context-check))))


(: compile-toplevel-set (ToplevelSet CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a toplevel mutation.
(define (compile-toplevel-set exp cenv target linkage)
  (let ([lexical-pos (make-EnvPrefixReference (ToplevelSet-depth exp)
                                              (ToplevelSet-pos exp))])
    (let ([get-value-code
           (compile (ToplevelSet-value exp) cenv lexical-pos
                    next-linkage/expects-single)]
          [singular-context-check (emit-singular-context linkage)])
      (end-with-linkage
       linkage
       cenv
       (append-instruction-sequences
        get-value-code
        (make-instruction-sequence 
         `(,(make-AssignImmediateStatement target (make-Const (void)))))
        singular-context-check)))))


(: compile-branch (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles a conditional branch.
(define (compile-branch exp cenv target linkage)
  (let: ([t-branch : Symbol (make-label 'trueBranch)]
         [f-branch : Symbol (make-label 'falseBranch)]
         [after-if : Symbol (make-label 'afterIf)])
        (let ([consequent-linkage
               (cond
                 [(NextLinkage? linkage)
                  (let ([context (NextLinkage-context linkage)])
                    (make-LabelLinkage after-if context))]
                 [(ReturnLinkage? linkage)
                  linkage]
                 [(LabelLinkage? linkage)
                  linkage])])
          (let ([p-code (compile (Branch-predicate exp) cenv 'val next-linkage/expects-single)]
                [c-code (compile (Branch-consequent exp) cenv target consequent-linkage)]
                [a-code (compile (Branch-alternative exp) cenv target linkage)])
            (append-instruction-sequences 
             p-code
             (append-instruction-sequences
              (make-instruction-sequence
               `(,(make-TestAndBranchStatement (make-TestFalse (make-Reg 'val))
                                               f-branch)))
              t-branch 
              c-code
              f-branch
              a-code
              after-if))))))


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
         (let* ([on-return/multiple (make-label 'beforePromptPopMultiple)]
                [on-return (make-LinkedLabel (make-label 'beforePromptPop)
                                                     on-return/multiple)])
           (end-with-linkage 
            linkage
            cenv
            (append-instruction-sequences 
             (make-instruction-sequence 
              `(,(make-PushControlFrame/Prompt
                  default-continuation-prompt-tag
                  on-return)))
             (compile (first seq) cenv 'val return-linkage/nontail)
             (emit-values-context-check-on-procedure-return (linkage-context linkage)
                                                            on-return/multiple
                                                            on-return)
	     (make-instruction-sequence
	      `(,(make-AssignImmediateStatement target (make-Reg 'val)))))))]
        [else
         (let* ([on-return/multiple (make-label 'beforePromptPopMultiple)]
                [on-return (make-LinkedLabel (make-label 'beforePromptPop)
                                                     on-return/multiple)])
           (append-instruction-sequences 
            (make-instruction-sequence
             `(,(make-PushControlFrame/Prompt
                 (make-DefaultContinuationPromptTag)
                 on-return)))
            (compile (first seq) cenv 'val return-linkage/nontail)
            on-return/multiple
            (make-instruction-sequence
             `(,(make-PopEnvironment (make-SubtractArg (make-Reg 'argcount)
                                                       (make-Const 1))
                                     (make-Const 0))))
            on-return
            (compile-splice (rest seq) cenv target linkage)))]))


(: compile-begin0 ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; FIXME: this is broken at the moment.
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
	       (make-instruction-sequence
		`(,(make-Comment "begin0")))
               ;; Evaluate the first expression in a multiple-value context, and get the values on the stack.
               (compile (first seq) cenv 'val next-linkage/keep-multiple-on-stack)
               (make-instruction-sequence
                `(,(make-TestAndBranchStatement (make-TestZero (make-Reg 'argcount)) after-first-seq)
                  ,(make-PushImmediateOntoEnvironment (make-Reg 'val) #f)))
               after-first-seq
               ;; At this time, the argcount values are on the stack.
               ;; Next, we save those values temporarily in a throwaway control frame.
               (make-instruction-sequence
                `(,(make-PushControlFrame/Generic)
                  ,(make-AssignImmediateStatement (make-ControlFrameTemporary 'pendingBegin0Count)
                                                  (make-Reg 'argcount))
                  ,(make-PerformStatement (make-UnspliceRestFromStack! (make-Const 0) (make-Reg 'argcount)))
                  ,(make-AssignImmediateStatement (make-ControlFrameTemporary 'pendingBegin0Values)
                                                  (make-EnvLexicalReference 0 #f))
                  ,(make-PopEnvironment (make-Const 1) (make-Const 0))))))]
            
            [reinstate-values-on-stack
             (let ([after-values-reinstated (make-label 'afterValuesReinstated)])
             (make-instruction-sequence
              `(;; Reinstate the values of the first expression, and drop the throwaway control frame.
                ,(make-PushImmediateOntoEnvironment (make-ControlFrameTemporary 'pendingBegin0Values) #f)
                ,(make-PerformStatement (make-SpliceListIntoStack! (make-Const 0)))
                ,(make-AssignImmediateStatement 'argcount (make-ControlFrameTemporary 'pendingBegin0Count))
                ,(make-PopControlFrame)
                ,(make-TestAndBranchStatement (make-TestZero (make-Reg 'argcount)) after-values-reinstated)
                ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
                ,(make-PopEnvironment (make-Const 1) (make-Const 0))
                ,after-values-reinstated)))])
       
       (append-instruction-sequences
        evaluate-and-save-first-expression        
        
        (compile-sequence (rest seq) cenv 'val next-linkage/drop-multiple)
        
        reinstate-values-on-stack        
        (make-instruction-sequence
         `(,(make-AssignImmediateStatement target (make-Reg 'val))))
        
        ;; TODO: context needs check for arguments.
        (cond
          [(ReturnLinkage? linkage)
           (cond
             [(ReturnLinkage-tail? linkage)
              (make-instruction-sequence 
               `(,(make-PopEnvironment (make-Const (length cenv)) 
                                       (make-SubtractArg (make-Reg 'argcount)
                                                         (make-Const 1)))
                 ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel/MultipleValueReturn))
                 ,(make-PopControlFrame)
                 ,(make-GotoStatement (make-Reg 'proc))))]
             [else
              (make-instruction-sequence
               `(,(make-AssignImmediateStatement 'proc (make-ControlStackLabel/MultipleValueReturn))
                 ,(make-PopControlFrame)
                 ,(make-GotoStatement (make-Reg 'proc))))])]
          
          [(NextLinkage? linkage)
           empty-instruction-sequence]
          
          [(LabelLinkage? linkage)
           (make-instruction-sequence 
            `(,(make-GotoStatement (make-Label (LabelLinkage-label linkage)))))])))]))
              
       


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
      (make-instruction-sequence 
       `(,(make-AssignPrimOpStatement 
           target
           (make-MakeCompiledProcedure (Lam-entry-label exp)
                                       (Lam-arity exp)
                                       (Lam-closure-map exp)
                                       (Lam-name exp)))))
      singular-context-check))))

(: compile-empty-closure-reference (EmptyClosureReference CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-empty-closure-reference exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (end-with-linkage
     linkage
     cenv
     (append-instruction-sequences      
      (make-instruction-sequence 
       `(,(make-AssignPrimOpStatement 
           target
           (make-MakeCompiledProcedure (EmptyClosureReference-entry-label exp)
                                       (EmptyClosureReference-arity exp)
                                       empty
                                       (EmptyClosureReference-name exp)))))
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
      (make-instruction-sequence
       `(,(make-Comment "scratch space for case-lambda")
         ,(make-PushEnvironment n #f)))
      
      ;; Compile each of the lambdas
      (apply append-instruction-sequences
             (map (lambda: ([lam : (U Lam EmptyClosureReference)]
                            [target : Target]) 
                           (make-instruction-sequence
                            `(,(make-AssignPrimOpStatement
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
                                                               (EmptyClosureReference-name lam))])))))
                  (CaseLam-clauses exp)
                  (build-list (length (CaseLam-clauses exp))
                              (lambda: ([i : Natural])
                                       (make-EnvLexicalReference i #f)))))
      
      ;; Make the case lambda as a regular compiled procedure.  Its closed values are the lambdas.
      (make-instruction-sequence 
       `(,(make-AssignPrimOpStatement 
           (adjust-target-depth target n)
           (make-MakeCompiledProcedure (CaseLam-entry-label exp)
                                       (merge-arities (map Lam-arity (CaseLam-clauses exp)))
                                       (build-list n (lambda: ([i : Natural]) i))
                                       (CaseLam-name exp)))
         
         ;; Finally, pop off the scratch space.
         ,(make-PopEnvironment (make-Const n) (make-Const 0))))
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
  (map (lambda: ([i : Natural]) (+ i n))
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
      (make-instruction-sequence 
       `(,(make-AssignPrimOpStatement 
           target
           (make-MakeCompiledProcedureShell (Lam-entry-label exp)
                                            (if (Lam-rest? exp)
                                                (make-ArityAtLeast (Lam-num-parameters exp))
                                                (Lam-num-parameters exp))
                                            (Lam-name exp)))))
      singular-context-check))))


(: compile-lambda-body (Lam CompileTimeEnvironment -> InstructionSequence))
;; Compiles the body of the lambda in the appropriate environment.
;; Closures will target their value to the 'val register, and use return linkage.
(define (compile-lambda-body exp cenv)
  (let: ([maybe-unsplice-rest-argument : InstructionSequence
                                       (if (Lam-rest? exp)
                                           (make-instruction-sequence 
                                            `(,(make-PerformStatement 
                                                (make-UnspliceRestFromStack! 
                                                 (make-Const (Lam-num-parameters exp))
                                                 (make-SubtractArg (make-Reg 'argcount)
                                                                   (make-Const (Lam-num-parameters exp)))))))
                                           empty-instruction-sequence)]
         [maybe-install-closure-values : InstructionSequence
                                       (if (not (empty? (Lam-closure-map exp)))
                                           (make-instruction-sequence 
                                            `(,(make-Comment (format "installing closure for ~s" (Lam-name exp)))
                                              ,(make-PerformStatement (make-InstallClosureValues!))))
                                           empty-instruction-sequence)]
         [lam-body-code : InstructionSequence
                        (compile (Lam-body exp)
                                 (extract-lambda-cenv exp cenv)
                                 'val
                                 return-linkage)])
        
        (append-instruction-sequences      
         (make-instruction-sequence 
          `(,(Lam-entry-label exp)))
         maybe-unsplice-rest-argument
         maybe-install-closure-values
         lam-body-code)))


(: compile-case-lambda-body (CaseLam CompileTimeEnvironment -> InstructionSequence))
(define (compile-case-lambda-body exp cenv)
  (append-instruction-sequences
   
   (make-instruction-sequence
    `(,(CaseLam-entry-label exp)))
   
   (apply append-instruction-sequences
          (map (lambda: ([lam : (U Lam EmptyClosureReference)]
                         [i : Natural])
                        (let ([not-match (make-label 'notMatch)])
                          (make-instruction-sequence
                           `(,(make-TestAndBranchStatement 
                               (make-TestClosureArityMismatch
                                (make-CompiledProcedureClosureReference 
                                 (make-Reg 'proc) 
                                 i)
                                (make-Reg 'argcount))
                               not-match)
                             ;; Set the procedure register to the lam
                             ,(make-AssignImmediateStatement 
                               'proc 
                               (make-CompiledProcedureClosureReference (make-Reg 'proc) i))
                             
                             ,(make-GotoStatement (make-Label
                                                   (cond [(Lam? lam)
                                                          (Lam-entry-label lam)]
                                                         [(EmptyClosureReference? lam)
                                                          (EmptyClosureReference-entry-label lam)])))
                             
                             ,not-match))))
               (CaseLam-clauses exp)
               (build-list (length (CaseLam-clauses exp)) (lambda: ([i : Natural]) i))))))

  
(: compile-lambda-bodies ((Listof lam+cenv) -> InstructionSequence))
;; Compile several lambda bodies, back to back.
(define (compile-lambda-bodies exps)
  (cond
    [(empty? exps)
     (make-instruction-sequence '())]
    [else
     (let: ([lam : (U Lam CaseLam) (lam+cenv-lam (first exps))]
            [cenv : CompileTimeEnvironment (lam+cenv-cenv (first exps))])
           (cond
             [(Lam? lam)
              (append-instruction-sequences (compile-lambda-body lam
                                                                 cenv)
                                            (compile-lambda-bodies (rest exps)))]
             [(CaseLam? lam)
              (append-instruction-sequences 
               (compile-case-lambda-body lam cenv)
               (compile-lambda-bodies (rest exps)))]))]))
              



(: extend-compile-time-environment/scratch-space (CompileTimeEnvironment Natural -> CompileTimeEnvironment))
(define (extend-compile-time-environment/scratch-space cenv n)
  (append (build-list n (lambda: ([i : Natural])
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
    
    (let: ([op-knowledge : CompileTimeEnvironmentEntry
                         (extract-static-knowledge (App-operator exp)
                                                   extended-cenv)])
          (cond
            [(eq? op-knowledge '?)
             (default)]
            [(ModuleVariable? op-knowledge)
             (cond
               [(symbol=? (ModuleLocator-name
                           (ModuleVariable-module-name op-knowledge))
                          '#%kernel)
                (let ([op (ModuleVariable-name op-knowledge)])
                  (cond [(KernelPrimitiveName/Inline? op)
                         (compile-kernel-primitive-application 
                          op
                          exp cenv target linkage)]
                        [else
                         (default)]))]
               [else
                (default)])]
            [(StaticallyKnownLam? op-knowledge)
             (compile-statically-known-lam-application op-knowledge exp cenv target linkage)]
            [(Prefix? op-knowledge)
             (error 'impossible)]
            [(Const? op-knowledge)
             (make-instruction-sequence 
              `(,(make-AssignImmediateStatement 'proc op-knowledge)
                ,(make-PerformStatement
                  (make-RaiseOperatorApplicationError! (make-Reg 'proc)))))]))))


(: compile-general-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-general-application exp cenv target linkage)
  (let* ([extended-cenv
          (extend-compile-time-environment/scratch-space 
           cenv 
           (length (App-operands exp)))]
         [proc-code (compile (App-operator exp)
                             extended-cenv 
                             (if (empty? (App-operands exp))
                                 'proc
                                 (make-EnvLexicalReference 
                                  (ensure-natural (sub1 (length (App-operands exp))))
                                  #f))
                             next-linkage/expects-single)]
         [operand-codes (map (lambda: ([operand : Expression]
                                       [target : Target])
                                      (compile operand
                                               extended-cenv
                                               target
                                               next-linkage/expects-single))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda: ([i : Natural])
                                                  (if (< i (sub1 (length (App-operands exp))))
                                                      (make-EnvLexicalReference i #f)
                                                      'val))))])    
    (append-instruction-sequences
     (make-instruction-sequence 
      `(,(make-Comment "scratch space for general application")
        ,(make-PushEnvironment (length (App-operands exp)) #f)))
     proc-code
     (juggle-operands operand-codes)
     (make-instruction-sequence `(,(make-AssignImmediateStatement 
                                    'argcount
                                    (make-Const (length (App-operands exp))))))
     (compile-general-procedure-call cenv 
                                     (make-Const (length (App-operands exp)))
                                     target
                                     linkage))))


(: compile-kernel-primitive-application
   (KernelPrimitiveName/Inline App CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; This is a special case of application, where the operator is statically
;; known to be in the set of hardcoded primitives.
;;
;; There's a special case optimization we can perform: we can avoid touching
;; the stack for constant arguments; rather than allocate (length (App-operands exp))
;; stack slots, we can do less than that.
;;
;; We have to be sensitive to mutation.
(define (compile-kernel-primitive-application kernel-op exp cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (cond
      ;; If all the arguments are primitive enough (all constants, localrefs, or toplevelrefs),
      ;; then application requires no stack space at all, and application is especially simple.
      [(andmap (lambda (op) 
                 ;; TODO: as long as the operand contains no applications?
                 (or (Constant? op)
                     (ToplevelRef? op)
                     (LocalRef? op)))
               (App-operands exp))
       (let* ([n (length (App-operands exp))]
              
              [operand-knowledge
               (map (lambda: ([arg : Expression])
                             (extract-static-knowledge 
                              arg 
                              (extend-compile-time-environment/scratch-space 
                               cenv n)))
                    (App-operands exp))]
              
              [typechecks?
               (map (lambda: ([dom : OperandDomain]
                              [known : CompileTimeEnvironmentEntry])
                             (not (redundant-check? dom known)))
                    (kernel-primitive-expected-operand-types kernel-op n)
                    operand-knowledge)]
              
              [expected-operand-types
               (kernel-primitive-expected-operand-types kernel-op n)]
              [operand-poss
               (simple-operands->opargs (map (lambda: ([op : Expression])
                                                      (adjust-expression-depth op n n))
                                             (App-operands exp)))])
         (end-with-linkage
          linkage cenv
          (append-instruction-sequences
           (make-instruction-sequence
            `(,(make-AssignPrimOpStatement
                target
                (make-CallKernelPrimitiveProcedure 
                 kernel-op 
                 operand-poss
                 expected-operand-types
                 typechecks?))))
           singular-context-check)))]
      
      [else
       ;; Otherwise, we can split the operands into two categories: constants, and the rest.
       (let*-values ([(n) 
                      (length (App-operands exp))]
                     
                     [(expected-operand-types)
                      (kernel-primitive-expected-operand-types kernel-op n)]
                     
                     [(constant-operands rest-operands)
                      (split-operands-by-constants 
                       (App-operands exp))]
                     
                     ;; here, we rewrite the stack references so they assume no scratch space
                     ;; used by the constant operands.
                     [(extended-cenv constant-operands rest-operands)
                      (values (extend-compile-time-environment/scratch-space 
                               cenv 
                               (length rest-operands))
                              
                              (map (lambda: ([constant-operand : Expression])
                                            (ensure-simple-expression
                                             (adjust-expression-depth constant-operand
                                                                      (length constant-operands)
                                                                      n)))
                                   constant-operands)
                              
                              (map (lambda: ([rest-operand : Expression])
                                            (adjust-expression-depth rest-operand
                                                                     (length constant-operands)
                                                                     n))
                                   rest-operands))]
                     
                     [(operand-knowledge)
                      (append (map (lambda: ([arg : Expression])
                                            (extract-static-knowledge arg extended-cenv))
                                   constant-operands)
                              (map (lambda: ([arg : Expression])
                                            (extract-static-knowledge arg extended-cenv))
                                   rest-operands))]
                     
                     [(typechecks?)
                      (map (lambda: ([dom : OperandDomain]
                                     [known : CompileTimeEnvironmentEntry])
                                    (not (redundant-check? dom known)))
                           (kernel-primitive-expected-operand-types kernel-op n)
                           operand-knowledge)]
                     
                     [(stack-pushing-code) 
                      (make-instruction-sequence `(,(make-PushEnvironment 
                                                     (length rest-operands)
                                                     #f)))]
                     [(stack-popping-code) 
                      (make-instruction-sequence `(,(make-PopEnvironment 
                                                     (make-Const (length rest-operands))
                                                     (make-Const 0))))]
                     
                     [(constant-operand-poss)
                      (simple-operands->opargs constant-operands)]
                     
                     [(rest-operand-poss)
                      (build-list (length rest-operands)
                                  (lambda: ([i : Natural])
                                           (make-EnvLexicalReference i #f)))]
                     [(rest-operand-code)
                      (apply append-instruction-sequences
                             (map (lambda: ([operand : Expression]
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
           (make-instruction-sequence
            `(,(make-AssignPrimOpStatement
                (adjust-target-depth target (length rest-operands))
                (make-CallKernelPrimitiveProcedure 
                 kernel-op 
                 (append constant-operand-poss rest-operand-poss)
                 expected-operand-types
                 typechecks?))))
           stack-popping-code
           singular-context-check)))])))




(: ensure-simple-expression (Expression -> (U Constant ToplevelRef LocalRef)))
(define (ensure-simple-expression e)
  (if (or (Constant? e)
          (LocalRef? e)
          (ToplevelRef? e))
      e
      (error 'ensure-simple-expression)))


(: simple-operands->opargs ((Listof Expression) -> (Listof OpArg)))
;; Produces a list of OpArgs if all the operands are particularly simple, and false therwise.
(define (simple-operands->opargs rands)
  (map (lambda: ([e : Expression])
                (cond
                  [(Constant? e)
                   (make-Const (Constant-v e))]
                  [(LocalRef? e)
                   (make-EnvLexicalReference (LocalRef-depth e)
                                             (LocalRef-unbox? e))]
                  [(ToplevelRef? e)
                   (make-EnvPrefixReference (ToplevelRef-depth e)
                                            (ToplevelRef-pos e))]
                  [else 
                   (error 'all-operands-are-constant "Impossible")]))
       rands))



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
              [(pair)
               (pair? (Const-const knowledge))])]
           [else
            #f])]))


(: split-operands-by-constants 
   ((Listof Expression)  -> 
                         (values (Listof (U Constant LocalRef ToplevelRef))
                                 (Listof Expression))))
;; Splits off the list of operations into two: a prefix of constant
;; or simple expressions, and the remainder.
;; TODO: if we can statically determine what arguments are immutable, regardless of
;; side effects, we can do a much better job here...
(define (split-operands-by-constants rands)
  (let: loop : (values (Listof (U Constant LocalRef ToplevelRef)) (Listof Expression))
        ([rands : (Listof Expression) rands]
         [constants : (Listof (U Constant LocalRef ToplevelRef))
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


(define-predicate natural? Natural)
(define-predicate atomic-arity-list? (Listof (U Natural ArityAtLeast)))

(: arity-matches? (Arity Natural -> Boolean))
(define (arity-matches? an-arity n)
  (cond
    [(natural? an-arity)
     (= an-arity n)]
    [(ArityAtLeast? an-arity)
     (>= n (ArityAtLeast-value an-arity))]
    [(atomic-arity-list? an-arity)
     (ormap (lambda: ([an-arity : (U Natural ArityAtLeast)])
                     (cond
                       [(natural? an-arity)
                        (= an-arity n)]
                       [(ArityAtLeast? an-arity)
                        (>= n (ArityAtLeast-value an-arity))]))
            an-arity)]))



(: compile-statically-known-lam-application 
   (StaticallyKnownLam App CompileTimeEnvironment Target Linkage 
                       -> InstructionSequence))
(define (compile-statically-known-lam-application static-knowledge exp cenv target linkage)
  (let ([arity-check
         (cond [(arity-matches? (StaticallyKnownLam-arity static-knowledge)
                                (length (App-operands exp)))
                empty-instruction-sequence]
               [else
                (make-PerformStatement
                 (make-RaiseArityMismatchError! 
                  (make-Reg 'proc)
                  (StaticallyKnownLam-arity static-knowledge)
                  (make-Const (length (App-operands exp)))))])])
    (let* ([extended-cenv 
            (extend-compile-time-environment/scratch-space 
             cenv 
             (length (App-operands exp)))]
           [proc-code (compile (App-operator exp)
                               extended-cenv 
                               (if (empty? (App-operands exp))
                                   'proc
                                   (make-EnvLexicalReference 
                                    (ensure-natural (sub1 (length (App-operands exp))))
                                    #f))
                               next-linkage/expects-single)]
           [operand-codes (map (lambda: ([operand : Expression]
                                         [target : Target])
                                        (compile operand 
                                                 extended-cenv 
                                                 target 
                                                 next-linkage/expects-single))
                               (App-operands exp)
                               (build-list (length (App-operands exp))
                                           (lambda: ([i : Natural])
                                                    (if (< i (sub1 (length (App-operands exp))))
                                                        (make-EnvLexicalReference i #f)
                                                        'val))))])    
      (append-instruction-sequences
       (make-instruction-sequence `(,(make-Comment "scratch space for statically known lambda application")
                                    ,(make-PushEnvironment (length (App-operands exp)) #f)))           
       proc-code
       (juggle-operands operand-codes)
       arity-check
       (compile-procedure-call/statically-known-lam static-knowledge 
                                                    cenv
                                                    extended-cenv 
                                                    (length (App-operands exp))
                                                    target
                                                    linkage)))))


(: juggle-operands ((Listof InstructionSequence) -> InstructionSequence))
;; Installs the operators.  At the end of this,
;; the procedure lives in 'proc, and the operands on the environment stack.
(define (juggle-operands operand-codes)
  (let: loop : InstructionSequence ([ops : (Listof InstructionSequence) operand-codes])
        (cond
          ;; If there are no operands, no need to juggle.
          [(null? ops)
           (make-instruction-sequence empty)]
          [(null? (rest ops))
           (let: ([n : Natural (ensure-natural (sub1 (length operand-codes)))])
                 ;; The last operand needs to be handled specially: it currently lives in 
                 ;; val.  We move the procedure at env[n] over to proc, and move the
                 ;; last operand at 'val into env[n].
                 (append-instruction-sequences 
                  (car ops)
                  (make-instruction-sequence 
                   `(,(make-AssignImmediateStatement 'proc 
                                                     (make-EnvLexicalReference n #f))
                     ,(make-AssignImmediateStatement (make-EnvLexicalReference n #f)
                                                     (make-Reg 'val))))))]
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
            'drop-multiple])]
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
  (let: ([primitive-branch : Symbol (make-label 'primitiveBranch)]
         [compiled-branch : Symbol (make-label 'compiledBranch)]
         [after-call : Symbol (make-label 'afterCall)])
        (let: ([compiled-linkage : Linkage (if (and (ReturnLinkage? linkage)
                                                    (ReturnLinkage-tail? linkage))
                                               linkage
                                               (make-LabelLinkage after-call
                                                                  (linkage-context linkage)))]
               [primitive-linkage : Linkage
                                  (make-NextLinkage (linkage-context linkage))])
              (append-instruction-sequences
               (make-instruction-sequence 
                `(,(make-TestAndBranchStatement (make-TestPrimitiveProcedure
                                                 (make-Reg 'proc))
                                                primitive-branch)))
               
               
               ;; Compiled branch
               compiled-branch
               (make-instruction-sequence
                `(,(make-PerformStatement (make-CheckClosureArity! (make-Reg 'argcount)))))
               (compile-compiled-procedure-application cenv
                                                       number-of-arguments
                                                       'dynamic
                                                       target
                                                       compiled-linkage)
               
               ;; Primitive branch
               primitive-branch
               (end-with-linkage
                linkage
                cenv
                (append-instruction-sequences
                 (make-instruction-sequence 
                  `(,(make-PerformStatement (make-CheckPrimitiveArity! (make-Reg 'argcount)))))
                 (compile-primitive-application cenv target primitive-linkage)
                 
                 after-call))))))



(: compile-primitive-application (CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-primitive-application cenv target linkage)
  (let ([singular-context-check (emit-singular-context linkage)])
    (append-instruction-sequences
     (make-instruction-sequence
      `(,(make-AssignPrimOpStatement 'val (make-ApplyPrimitiveProcedure))
        ,(make-PopEnvironment (make-Reg 'argcount)
                              (make-Const 0))
        ,@(if (eq? target 'val)
              empty
              (list (make-AssignImmediateStatement target (make-Reg 'val))))))
     singular-context-check)))



(: compile-procedure-call/statically-known-lam 
   (StaticallyKnownLam CompileTimeEnvironment CompileTimeEnvironment Natural Target Linkage -> InstructionSequence))
(define (compile-procedure-call/statically-known-lam static-knowledge cenv extended-cenv n target linkage)
  (let*: ([after-call : Symbol (make-label 'afterCall)]
          [compiled-linkage : Linkage (if (and (ReturnLinkage? linkage)
                                               (ReturnLinkage-tail? linkage))
                                          linkage
                                          (make-LabelLinkage 
                                           after-call
                                           (linkage-context linkage)))])
         (append-instruction-sequences
          (make-instruction-sequence `(,(make-AssignImmediateStatement 
                                         'argcount
                                         (make-Const n))))
          (compile-compiled-procedure-application cenv
                                                  (make-Const n)
                                                  (make-Label 
                                                   (StaticallyKnownLam-entry-point static-knowledge))
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
  (let* ([entry-point-target
          ;; Optimization: if the entry-point is known to be a static label,
          ;; use that.  Otherwise, grab the entry point from the proc register.
          (cond [(Label? entry-point)
                 entry-point]
                [(eq? entry-point 'dynamic)
                 (make-CompiledProcedureEntry (make-Reg 'proc))])]
         
         ;; If the target isn't val, migrate the value from val into it.
         [maybe-migrate-val-to-target
          (cond
            [(eq? target 'val)
             empty-instruction-sequence]
            [else
             (make-instruction-sequence
              `(,(make-AssignImmediateStatement target (make-Reg 'val))))])]
         
         [on-return/multiple (make-label 'procReturnMultiple)]
         
         [on-return (make-LinkedLabel (make-label 'procReturn)
                                        on-return/multiple)]
         
         ;; This code does the initial jump into the procedure.  Clients of this code
         ;; are expected to generate the proc-return-multiple and proc-return code afterwards.
         [nontail-jump-into-procedure
          (append-instruction-sequences 
           (make-instruction-sequence 
            `(,(make-PushControlFrame/Call on-return)
              ,(make-GotoStatement entry-point-target))))])
    
    (cond [(ReturnLinkage? linkage)
           (cond
             [(eq? target 'val)
              (cond
                [(ReturnLinkage-tail? linkage)
                 ;; This case happens when we're in tail position.
                 ;; We clean up the stack right before the jump, and do not add
                 ;; to the control stack.
                 (let ([reuse-the-stack
                        (make-instruction-sequence 
                         `(,(make-PopEnvironment (make-Const (length cenv))
                                                 number-of-arguments)))])
                   (append-instruction-sequences
                    reuse-the-stack
                    (make-instruction-sequence
                     `(;; Assign the proc value of the existing call frame.
                       ,(make-PerformStatement (make-SetFrameCallee! (make-Reg 'proc)))
                       ,(make-GotoStatement entry-point-target)))))]
                
                [else	    
                 ;; This case happens when we should be returning to a caller, but where
                 ;; we are not in tail position.
                 (append-instruction-sequences
                  nontail-jump-into-procedure
                  on-return/multiple
                  (make-instruction-sequence
                   `(,(make-PopEnvironment (make-SubtractArg (make-Reg 'argcount)
                                                             (make-Const 1))
                                           (make-Const 0))))
                  on-return)])]
             
             [else
              (error 'compile "return linkage, target not val: ~s" target)])]
          
          
          [(or (NextLinkage? linkage) (LabelLinkage? linkage))
           (let* ([context (linkage-context linkage)]
                  
                  [check-values-context-on-procedure-return
                   (emit-values-context-check-on-procedure-return context on-return/multiple on-return)]
                  
                  [maybe-jump-to-label
                   (if (LabelLinkage? linkage)
                       (make-instruction-sequence
                        `(,(make-GotoStatement (make-Label (LabelLinkage-label linkage)))))
                       empty-instruction-sequence)])

             (append-instruction-sequences
              nontail-jump-into-procedure
              check-values-context-on-procedure-return
              maybe-migrate-val-to-target
              maybe-jump-to-label))])))



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
      (make-instruction-sequence
       `(,(make-PopEnvironment (SubtractArg (make-Reg 'argcount) (make-Const 1))
                               (make-Const 0))))
      on-return)]
    
    [(eq? context 'keep-multiple)
     (let ([after-return (make-label 'afterReturn)])
       (append-instruction-sequences
        on-return/multiple
        (make-instruction-sequence
         `(,(make-GotoStatement (make-Label after-return))))
        on-return
        (make-instruction-sequence
         `(,(make-AssignImmediateStatement 'argcount (make-Const 1))))
        after-return))]
    
    [(natural? context)
     (cond
       [(= context 1)
        (append-instruction-sequences
         on-return/multiple
         (make-instruction-sequence
          `(,(make-PerformStatement
              (make-RaiseContextExpectedValuesError! 1))))
         on-return)]
       [else
        (let ([after-value-check (make-label 'afterValueCheck)])
          (append-instruction-sequences
           on-return/multiple
           (make-instruction-sequence
            `(
              ;; if the wrong number of arguments come in, die
              ,(make-TestAndBranchStatement
                (make-TestZero (make-SubtractArg (make-Reg 'argcount)
                                                 (make-Const context)))
                after-value-check)))
           on-return
           (make-instruction-sequence
            `(,(make-PerformStatement
                (make-RaiseContextExpectedValuesError! context))))
           after-value-check))])]))




(: extract-static-knowledge (Expression CompileTimeEnvironment ->  
                                        CompileTimeEnvironmentEntry))
;; Statically determines what we know about the expression, given the compile time environment.
;; We should do more here eventually, including things like type inference or flow analysis, so that
;; we can generate better code.
(define (extract-static-knowledge exp cenv)
  (cond
    [(Lam? exp)
     (make-StaticallyKnownLam (Lam-name exp)
                              (Lam-entry-label exp)
                              (if (Lam-rest? exp)
                                  (make-ArityAtLeast (Lam-num-parameters exp))
                                  (Lam-num-parameters exp)))]
    [(and (LocalRef? exp) 
          (not (LocalRef-unbox? exp)))
     (let ([entry (list-ref cenv (LocalRef-depth exp))])
       entry)]
    
    [(ToplevelRef? exp)
     (let: ([name : (U Symbol False GlobalBucket ModuleVariable)
                  (list-ref (Prefix-names (ensure-prefix (list-ref cenv (ToplevelRef-depth exp))))
                            (ToplevelRef-pos exp))])
           (cond
             [(ModuleVariable? name)
              name]
             [(GlobalBucket? name)
              '?]
             [else
              '?]))]
    
    [(Constant? exp)
     (make-Const (Constant-v exp))]
    
    [else
     '?]))


(: compile-let1 (Let1 CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Single value binding.  Since there's only one rhs, we have more static guarantees we can make,
;; which is why we can use extract-static-knowledge here.
(define (compile-let1 exp cenv target linkage)
  (let*: ([rhs-code : InstructionSequence 
                    (compile (Let1-rhs exp)
                             (cons '? cenv)
                             (make-EnvLexicalReference 0 #f)
                             next-linkage/expects-single)]
          [after-let1 : Symbol (make-label 'afterLetOne)]
          [after-body-code : Symbol (make-label 'afterLetBody)]
          [extended-cenv : CompileTimeEnvironment (cons (extract-static-knowledge (Let1-rhs exp)
                                                                                  (cons '? cenv))
                                                        cenv)]
          [let-linkage : Linkage
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
          [body-target : Target (adjust-target-depth target 1)]
          [body-code : InstructionSequence
                     (compile (Let1-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences
           (make-instruction-sequence `(,(make-Comment "scratch space for let1")
                                        ,(make-PushEnvironment 1 #f)))
           rhs-code
           body-code
           after-body-code
           (make-instruction-sequence `(,(make-PopEnvironment (make-Const 1) (make-Const 0))))
           after-let1))))




(: compile-let-void (LetVoid CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Binding several values.  Unlike before, it has less knowledge about what values will be bound,
;; and so there's less analysis here.
(define (compile-let-void exp cenv target linkage)
  (let*: ([n : Natural (LetVoid-count exp)]
          [after-let : Symbol (make-label 'afterLet)]
          [after-body-code : Symbol (make-label 'afterLetBody)]
          [extended-cenv : CompileTimeEnvironment (append (build-list (LetVoid-count exp) 
                                                                      (lambda: ([i : Natural]) '?))
                                                          cenv)]
          [let-linkage : Linkage
                       (cond
                         [(NextLinkage? linkage)
                          linkage]
                         [(ReturnLinkage? linkage)
                          (cond
                            [(ReturnLinkage-tail? linkage)
                             linkage]
                            [else
                             (make-LabelLinkage after-body-code (linkage-context linkage))])]
                         [(LabelLinkage? linkage)
                          (make-LabelLinkage after-body-code (LabelLinkage-context linkage))])]
          [body-target : Target (adjust-target-depth target n)]
          [body-code : InstructionSequence
                     (compile (LetVoid-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences 
           (make-instruction-sequence 
            `(,(make-Comment "scratch space for let-void")
              ,(make-PushEnvironment n (LetVoid-boxes? exp))))
           body-code
           after-body-code
           (make-instruction-sequence 
            `(,(make-PopEnvironment (make-Const n) (make-Const 0))))
           after-let))))



(: compile-let-rec (LetRec CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiled recursive Lams.  Each lambda is installed as a shell, and then the closures
;; are installed in-place.
(define (compile-let-rec exp cenv target linkage)
  (let*: ([n : Natural (length (LetRec-procs exp))]
          [extended-cenv : CompileTimeEnvironment
                         (append (map (lambda: ([p : Lam])
                                               (extract-static-knowledge 
                                                p
                                                (append (build-list (length (LetRec-procs exp))
                                                                    (lambda: ([i : Natural])
                                                                             '?))
                                                        (drop cenv n))))
                                      (LetRec-procs exp))
                                 (drop cenv n))]
          [n : Natural (length (LetRec-procs exp))]
          [after-body-code : Symbol (make-label 'afterBodyCode)]
          [letrec-linkage : Linkage (cond
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
                  (map (lambda: ([lam : Lam]
                                 [i : Natural])
                                (compile-lambda-shell lam 
                                                      extended-cenv
                                                      (make-EnvLexicalReference i #f) 
                                                      next-linkage/expects-single))
                       (LetRec-procs exp)
                       (build-list n (lambda: ([i : Natural]) i))))
           
           ;; Fix the closure maps of each
           (apply append-instruction-sequences
                  (map (lambda: ([lam : Lam]
                                 [i : Natural])
                                (make-instruction-sequence 
                                 `(,(make-Comment (format "Installing shell for ~s\n" (Lam-name lam)))
                                   ,(make-PerformStatement 
                                     (make-FixClosureShellMap! i (Lam-closure-map lam))))))
                       
                       (LetRec-procs exp)
                       (build-list n (lambda: ([i : Natural]) i))))
           
           ;; Compile the body
           (compile (LetRec-body exp) extended-cenv target letrec-linkage)
           
           after-body-code))))



(: compile-install-value (InstallValue CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-install-value exp cenv target linkage)
  (append-instruction-sequences
   (make-instruction-sequence `(,(make-Comment "install-value")))
   (let ([count (InstallValue-count exp)])
     (cond [(= count 0)
	    (end-with-linkage
	     linkage
	     cenv
	     (compile (InstallValue-body exp)
		      cenv
		      target
		      (make-NextLinkage 0)))]
	   [(= count 1)
	    (append-instruction-sequences
	     (make-instruction-sequence 
	      `(,(make-Comment (format "installing single value into ~s"
				       (InstallValue-depth exp)))))
	     (end-with-linkage
	      linkage
	      cenv
	      (compile (InstallValue-body exp)
		       cenv
		       (make-EnvLexicalReference (InstallValue-depth exp) (InstallValue-box? exp))
		       (make-NextLinkage 1))))]
	   [else
	    (end-with-linkage
	     linkage
	     cenv
	     (append-instruction-sequences
	      (make-instruction-sequence 
	       `(,(make-Comment "install-value: evaluating values")))
	      (compile (InstallValue-body exp)
		       cenv
		       'val
		       (make-NextLinkage count))
	      (apply append-instruction-sequences
		     (map (lambda: ([to : EnvLexicalReference]
				    [from : OpArg])
				   (make-instruction-sequence
				    `(,(make-Comment "install-value: installing value")
				      ,(make-AssignImmediateStatement to from))))
			  (build-list count (lambda: ([i : Natural])
						     (make-EnvLexicalReference (+ i 
										  (InstallValue-depth exp)
										  (sub1 count))
									       (InstallValue-box? exp))))
			  (cons (make-Reg 'val) 
				(build-list (sub1 count) (lambda: ([i : Natural])
								  (make-EnvLexicalReference i #f))))))
	      (make-instruction-sequence
	       `(,(make-PopEnvironment (make-Const (sub1 count)) (make-Const 0))))))]))))



(: compile-box-environment-value (BoxEnv CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-box-environment-value exp cenv target linkage)
  (append-instruction-sequences
   (make-instruction-sequence 
    `(,(make-AssignPrimOpStatement (make-EnvLexicalReference (BoxEnv-depth exp) #f)
                                   (make-MakeBoxedEnvironmentValue (BoxEnv-depth exp)))))
   (compile (BoxEnv-body exp) cenv target linkage)))




(: compile-with-cont-mark (WithContMark CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-with-cont-mark exp cenv target linkage)
  
  (: in-return-context (-> InstructionSequence))
  (define (in-return-context)
    (append-instruction-sequences
     (compile (WithContMark-key exp) cenv 'val next-linkage/expects-single)
     (make-instruction-sequence
      `(,(make-AssignImmediateStatement
          (make-ControlFrameTemporary 'pendingContinuationMarkKey)
          (make-Reg 'val))))
     (compile (WithContMark-value exp) cenv 'val next-linkage/expects-single)
     (make-instruction-sequence
      `(,(make-PerformStatement (make-InstallContinuationMarkEntry!))))
     (compile (WithContMark-body exp) cenv target linkage)))
  
  (: in-other-context ((U NextLinkage LabelLinkage) -> InstructionSequence))
  (define (in-other-context linkage)
    (let ([body-next-linkage (cond [(NextLinkage? linkage)
                                    linkage]
                                   [(LabelLinkage? linkage)
                                    (make-NextLinkage (LabelLinkage-context linkage))])])
      (end-with-linkage 
       linkage cenv
       (append-instruction-sequences
        ;; Making a continuation frame; isn't really used for anything
        ;; but recording the key/value data.
        (make-instruction-sequence 
         `(,(make-PushControlFrame/Generic)))
        (compile (WithContMark-key exp) cenv 'val next-linkage/expects-single)
        (make-instruction-sequence `(,(make-AssignImmediateStatement
                                       (make-ControlFrameTemporary 'pendingContinuationMarkKey)
                                       (make-Reg 'val))))
        (compile (WithContMark-value exp) cenv 'val next-linkage/expects-single)
        (make-instruction-sequence `(,(make-PerformStatement
                                       (make-InstallContinuationMarkEntry!))))
        (compile (WithContMark-body exp) cenv target body-next-linkage)
        (make-instruction-sequence
         `(,(make-PopControlFrame)))))))
  
  (cond
    [(ReturnLinkage? linkage)
     (in-return-context)]
    [(NextLinkage? linkage) 
     (in-other-context linkage)]
    [(LabelLinkage? linkage)
     (in-other-context linkage)]))


(: compile-apply-values (ApplyValues CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-apply-values exp cenv target linkage)
  (let ([on-zero (make-label 'onZero)]
        [after-args-evaluated (make-label 'afterArgsEvaluated)])
    (append-instruction-sequences
     
     ;; Save the procedure value temporarily in a control stack frame
     (make-instruction-sequence 
      `(,(make-PushControlFrame/Generic)))
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
     
     (make-instruction-sequence
      `(,(make-TestAndBranchStatement (make-TestZero (make-Reg 'argcount)) after-args-evaluated)
        ;; In the common case where we do get values back, we push val onto the stack too,
        ;; so that we have n values on the stack before we jump to the procedure call.
        ,(make-PushImmediateOntoEnvironment (make-Reg 'val) #f)))
     
     after-args-evaluated
     ;; Retrieve the procedure off the temporary control frame.
     (make-instruction-sequence
      `(,(make-AssignImmediateStatement 
          'proc 
          (make-ControlFrameTemporary 'pendingApplyValuesProc))))
     
     ;; Pop off the temporary control frame
     (make-instruction-sequence 
      `(,(make-PopControlFrame)))
     
     ;; Finally, do the generic call into the function.
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
             (map (lambda: ([id : ToplevelRef]
                            [from : OpArg])
                           (make-instruction-sequence
                            `(,(make-AssignImmediateStatement
                                ;; Slightly subtle: the toplevelrefs were with respect to the
                                ;; stack at the beginning of def-values, but at the moment,
                                ;; there may be additional values that are currently there.
                                (make-EnvPrefixReference (+ (ensure-natural (sub1 n))
                                                            (ToplevelRef-depth id))
                                                         (ToplevelRef-pos id))
                                from))))
                  ids
                  (if (> n 0) 
                      (cons (make-Reg 'val)
                            (build-list (sub1 n)
                                        (lambda: ([i : Natural])
                                                 (make-EnvLexicalReference i #f))))
                      empty)))
          empty-instruction-sequence)

      ;; Finally, make sure any multiple values are off the stack.
      (if (> (length ids) 1)
          (make-instruction-sequence 
           `(,(make-PopEnvironment (make-Const (sub1 (length ids)))
                                   (make-Const 0))))
          empty-instruction-sequence)))))
           


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
                            (make-AssignImmediateStatement target exp)
                            singular-context-check)))]
      [else
       ;; Maybe warn about the unimplemented kernel primitive.
       (unless (set-contains? (current-seen-unimplemented-kernel-primitives)
                              id)
         (set-insert! (current-seen-unimplemented-kernel-primitives)
                              id)
         ((current-warn-unimplemented-kernel-primitive) id))
       
       (make-PerformStatement (make-RaiseUnimplementedPrimitiveError! id))])))






(: append-instruction-sequences (InstructionSequence * -> InstructionSequence))
(define (append-instruction-sequences . seqs)
  (append-seq-list seqs))

(: append-2-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (append-2-sequences seq1 seq2)
  (make-instruction-sequence
   (append (statements seq1) (statements seq2))))

(: append-seq-list ((Listof InstructionSequence) -> InstructionSequence))
(define (append-seq-list seqs)
  (if (null? seqs)
      empty-instruction-sequence
      (append-2-sequences (car seqs)
                          (append-seq-list (cdr seqs)))))


(: ensure-natural (Integer -> Natural))
(define (ensure-natural n)
  (if (>= n 0)
      n
      (error 'ensure-natural "Not a natural: ~s\n" n)))

(: ensure-prefix (CompileTimeEnvironmentEntry -> Prefix))
(define (ensure-prefix x)
  (if (Prefix? x)
      x
      (error 'ensure-prefix "Not a prefix: ~s" x)))

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
                              (EnvPrefixReference-pos target))]
    [(PrimitivesReference? target)
     target]
    [(ControlFrameTemporary? target)
     target]
    [(ModulePrefixTarget? target)
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
                  (adjust-expression-depth (Module-code exp) n (add1 skip)))]
    
    [(Constant? exp)
     exp]
    
    [(ToplevelRef? exp)
     (if (< (ToplevelRef-depth exp) skip)
         exp
         (make-ToplevelRef (ensure-natural (- (ToplevelRef-depth exp) n))
                           (ToplevelRef-pos exp)))]
    
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
               (map (lambda: ([d : Natural]) 
                             (if (< d skip)
                                 d
                                 (ensure-natural (- d n))))
                    (Lam-closure-map exp))
               (Lam-entry-label exp))]
    
    [(CaseLam? exp)
     (make-CaseLam (CaseLam-name exp)
                   (map (lambda: ([lam : (U Lam EmptyClosureReference)])
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
     (make-Seq (map (lambda: ([action : Expression])
                             (adjust-expression-depth action n skip))
                    (Seq-actions exp)))]
    
    [(Splice? exp)
     (make-Splice (map (lambda: ([action : Expression])
                                (adjust-expression-depth action n skip))
                       (Splice-actions exp)))]

    [(Begin0? exp)
     (make-Begin0 (map (lambda: ([action : Expression])
                                (adjust-expression-depth action n skip))
                       (Begin0-actions exp)))]
    
    [(App? exp)
     (make-App (adjust-expression-depth (App-operator exp) n 
                                        (+ skip (length (App-operands exp))))
               (map (lambda: ([operand : Expression])
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
     (make-LetRec (let: loop : (Listof Lam) ([procs : (Listof Lam) (LetRec-procs exp)])
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
     (make-DefValues (map (lambda: ([id : ToplevelRef])
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