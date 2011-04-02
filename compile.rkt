#lang typed/racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "kernel-primitives.rkt"
         racket/bool
         racket/list)

(provide (rename-out [-compile compile])
         compile-general-procedure-call
         append-instruction-sequences
         adjust-target-depth)





(: -compile (Expression Target Linkage -> (Listof Statement)))
;; Generates the instruction-sequence stream.
;; Note: the toplevel generates the lambda body streams at the head, and then the
;; rest of the instruction stream.
(define (-compile exp target linkage)
  (let ([after-lam-bodies (make-label 'afterLamBodies)]
        [before-pop-prompt (make-label 'beforePopPrompt)])
    (statements
     (append-instruction-sequences 
      
      (make-instruction-sequence 
       `(,(make-GotoStatement (make-Label after-lam-bodies))))
      (compile-lambda-bodies (collect-all-lams exp))
      after-lam-bodies
      
      (make-instruction-sequence
       `(,(make-PushControlFrame/Prompt default-continuation-prompt-tag
                                        before-pop-prompt)))
      (compile exp '() target prompt-linkage)
      before-pop-prompt))))

(define-struct: lam+cenv ([lam : Lam]
                          [cenv : CompileTimeEnvironment]))


(: collect-all-lams (Expression -> (Listof lam+cenv)))
;; Finds all the lambdas in the expression.
(define (collect-all-lams exp)
  (let: loop : (Listof lam+cenv)
        ([exp : Expression exp]
         [cenv : CompileTimeEnvironment '()])
        
        (cond
          [(Top? exp)
           (loop (Top-code exp) (cons (Top-prefix exp) cenv))]
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
          [(Seq? exp)
           (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                              (Seq-actions exp)))]
          [(Splice? exp)
           (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                              (Splice-actions exp)))]
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
           (let ([new-cenv (append (map (lambda: ([p : Lam]) 
                                                 (extract-static-knowledge 
                                                  p 
                                                  (append (build-list (length (LetRec-procs exp))
                                                                      (lambda: ([i : Natural]) '?))
                                                          cenv)))
                                        (reverse (LetRec-procs exp)))
                                   cenv)])
             (append (apply append 
                            (map (lambda: ([lam : Lam])
                                          (loop lam new-cenv))
                                 (LetRec-procs exp)))
                     (loop (LetRec-body exp) new-cenv)))])))



(: extract-lambda-cenv (Lam CompileTimeEnvironment -> CompileTimeEnvironment))
(define (extract-lambda-cenv lam cenv)
  (append (map (lambda: ([d : Natural])
                        (list-ref cenv d))
               (Lam-closure-map lam))
          (build-list (Lam-num-parameters lam) (lambda: ([i : Natural]) '?))))





(: compile (Expression CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles an expression into an instruction sequence.
(define (compile exp cenv target linkage)
  (cond
    [(Top? exp)
     (compile-top exp cenv target linkage)]
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
     (compile-let-rec exp cenv target linkage)]))




(: compile-top (Top CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-top top cenv target linkage)
  (let*: ([names : (Listof (U Symbol ModuleVariable False)) (Prefix-names (Top-prefix top))])
         (end-with-linkage 
          linkage cenv
          (append-instruction-sequences
           (make-instruction-sequence 
            `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! names))))
           (compile (Top-code top) (cons (Top-prefix top) cenv) target next-linkage)
           (make-instruction-sequence
            `(,(make-PopEnvironment 1 0)))))))



;; Add linkage for expressions.
(: end-with-linkage (Linkage CompileTimeEnvironment InstructionSequence -> InstructionSequence))
(define (end-with-linkage linkage cenv instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-linkage cenv linkage)))




(: compile-linkage (CompileTimeEnvironment Linkage -> InstructionSequence))
(define (compile-linkage cenv linkage)
  (cond
    [(ReturnLinkage? linkage)
     (make-instruction-sequence `(,(make-AssignPrimOpStatement 'proc (make-GetControlStackLabel))
                                  ,(make-PopEnvironment (length cenv) 0)
                                  ,(make-PopControlFrame)
                                  ,(make-GotoStatement (make-Reg 'proc))))]
    [(PromptLinkage? linkage)
     (make-instruction-sequence `(,(make-AssignPrimOpStatement 'proc (make-GetControlStackLabel))
                                  ,(make-PopControlFrame/Prompt)
                                  ,(make-GotoStatement (make-Reg 'proc))))]
    [(NextLinkage? linkage)
     empty-instruction-sequence]
    [(LabelLinkage? linkage)
     (make-instruction-sequence `(,(make-GotoStatement (make-Label (LabelLinkage-label linkage)))))]))



(: compile-constant (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-constant exp cenv target linkage)
  (end-with-linkage linkage
                    cenv
                    (make-instruction-sequence
                     `(,(make-AssignImmediateStatement target (make-Const (Constant-v exp)))))))



(: compile-local-reference (LocalRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-local-reference exp cenv target linkage)
  (end-with-linkage linkage
                    cenv
                    (make-instruction-sequence
                     `(,(make-AssignImmediateStatement 
                         target
                         (make-EnvLexicalReference (LocalRef-depth exp)
                                                   (LocalRef-unbox? exp)))))))


(: compile-toplevel-reference (ToplevelRef CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-toplevel-reference exp cenv target linkage)
  (end-with-linkage linkage
                    cenv
                    (make-instruction-sequence
                     `(,(make-PerformStatement (make-CheckToplevelBound!
                                                (ToplevelRef-depth exp)
                                                (ToplevelRef-pos exp)))
                       ,(make-AssignImmediateStatement 
                         target
                         (make-EnvPrefixReference (ToplevelRef-depth exp)
                                                  (ToplevelRef-pos exp)))))))


(: compile-toplevel-set (ToplevelSet CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-toplevel-set exp cenv target linkage)
  (let* ([var (ToplevelSet-name exp)]
         [lexical-pos (make-EnvPrefixReference (ToplevelSet-depth exp)
                                               (ToplevelSet-pos exp))])
    (let ([get-value-code
           (compile (ToplevelSet-value exp) cenv lexical-pos
                    next-linkage)])
      (end-with-linkage
       linkage
       cenv
       (append-instruction-sequences
        get-value-code
        (make-instruction-sequence `(,(make-AssignImmediateStatement target (make-Const (void))))))))))


(: compile-branch (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-branch exp cenv target linkage)
  (let: ([t-branch : LabelLinkage (make-LabelLinkage (make-label 'trueBranch))]
         [f-branch : LabelLinkage (make-LabelLinkage (make-label 'falseBranch))]
         [after-if : LabelLinkage (make-LabelLinkage (make-label 'afterIf))])
        (let ([consequent-linkage
               (if (eq? linkage next-linkage)
                   after-if
                   linkage)])
          (let ([p-code (compile (Branch-predicate exp) cenv 'val next-linkage)]
                [c-code (compile (Branch-consequent exp) cenv target consequent-linkage)]
                [a-code (compile (Branch-alternative exp) cenv target linkage)])
            (append-instruction-sequences p-code
                                          (append-instruction-sequences
                                           (make-instruction-sequence
                                            `(,(make-TestAndBranchStatement 'false? 
                                                                            'val
                                                                            (LabelLinkage-label f-branch))))
                                           (append-instruction-sequences
                                            (append-instruction-sequences (LabelLinkage-label t-branch) c-code)
                                            (append-instruction-sequences (LabelLinkage-label f-branch) a-code))
                                           (LabelLinkage-label after-if)))))))


(: compile-sequence ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-sequence seq cenv target linkage) 
  ;; All but the last will use next-linkage linkage.
  (if (last-exp? seq)
      (compile (first-exp seq) cenv target linkage)
      (append-instruction-sequences (compile (first-exp seq) cenv target next-linkage)
                                    (compile-sequence (rest-exps seq) cenv target linkage))))


(: compile-splice ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Wrap a continuation prompt around each of the expressions.
(define (compile-splice seq cenv target linkage) 
  (cond [(last-exp? seq)
         (let ([before-pop-prompt (make-label 'beforePromptPop)])
           (end-with-linkage 
            linkage
            cenv
            (append-instruction-sequences 
             (make-instruction-sequence `(,(make-PushControlFrame/Prompt
                                            default-continuation-prompt-tag
                                            before-pop-prompt)))
             (compile (first-exp seq) cenv target prompt-linkage)
             before-pop-prompt)))]
        [else
         (let ([before-pop-prompt (make-label 'beforePromptPop)])
           (append-instruction-sequences 
            (make-instruction-sequence `(,(make-PushControlFrame/Prompt
                                           (make-DefaultContinuationPromptTag)
                                           before-pop-prompt)))
            (compile (first-exp seq) cenv target prompt-linkage)
            before-pop-prompt
            (compile-splice (rest-exps seq) cenv target linkage)))]))



(: compile-lambda (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Write out code for lambda expressions.
;; The lambda will close over the free variables.
;; Assumption: all of the lambda bodies have already been written out at the top, in -compile.
(define (compile-lambda exp cenv target linkage) 
  (end-with-linkage 
   linkage
   cenv
   (make-instruction-sequence 
    `(,(make-AssignPrimOpStatement 
        target
        (make-MakeCompiledProcedure (Lam-entry-label exp)
                                    (Lam-num-parameters exp)
                                    (Lam-closure-map exp)
                                    (Lam-name exp)))))))


(: compile-lambda-shell (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Write out code for lambda expressions, minus the closure map.
;; Assumption: all of the lambda bodies have already been written out at the top, in -compile.
(define (compile-lambda-shell exp cenv target linkage) 
  (end-with-linkage 
   linkage
   cenv
   (make-instruction-sequence 
    `(,(make-AssignPrimOpStatement 
        target
        (make-MakeCompiledProcedureShell (Lam-entry-label exp)
                                         (Lam-num-parameters exp)
                                         (Lam-name exp)))))))


(: compile-lambda-body (Lam CompileTimeEnvironment -> InstructionSequence))
;; Compiles the body of the lambda in the appropriate environment.
(define (compile-lambda-body exp cenv)
  (append-instruction-sequences
   
   (make-instruction-sequence 
    `(,(Lam-entry-label exp)))
   
   (if (not (empty? (Lam-closure-map exp)))
       (make-instruction-sequence `(,(make-PerformStatement (make-InstallClosureValues!))))
       empty-instruction-sequence)
   
   (compile (Lam-body exp)
            (append (map (lambda: ([d : Natural]) 
                                  (list-ref cenv d))
                         (Lam-closure-map exp))
                    ;; fixme: We need to capture the cenv so we can maintain static knowledge
                    (build-list (Lam-num-parameters exp) (lambda: ([i : Natural]) '?)))
            'val
            return-linkage)))



(: compile-lambda-bodies ((Listof lam+cenv) -> InstructionSequence))
;; Compile several lambda bodies, back to back.
(define (compile-lambda-bodies exps)
  (cond
    [(empty? exps)
     (make-instruction-sequence '())]
    [else
     (append-instruction-sequences (compile-lambda-body (lam+cenv-lam (first exps))
                                                        (lam+cenv-cenv (first exps)))
                                   (compile-lambda-bodies (rest exps)))]))

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
               [(symbol=? (ModuleVariable-module-path op-knowledge) '#%kernel)
                (let ([op (ModuleVariable-name op-knowledge)])
                  (cond [(KernelPrimitiveName? op)
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
             (error 'application "Can't apply constant ~s as a function" (Const-const op-knowledge))]))))


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
                             next-linkage)]
         [operand-codes (map (lambda: ([operand : Expression]
                                       [target : Target])
                                      (compile operand extended-cenv target next-linkage))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda: ([i : Natural])
                                                  (if (< i (sub1 (length (App-operands exp))))
                                                      (make-EnvLexicalReference i #f)
                                                      'val))))])    
    (append-instruction-sequences
     (if (not (empty? (App-operands exp)))
         (make-instruction-sequence `(,(make-PushEnvironment (length (App-operands exp)) #f)))
         empty-instruction-sequence)
     proc-code
     (juggle-operands operand-codes)
     (compile-general-procedure-call cenv 
                                     extended-cenv 
                                     (length (App-operands exp))
                                     target
                                     linkage))))


(: compile-kernel-primitive-application
   (KernelPrimitiveName App CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; This is a special case of application, where the operator is statically
;; known to be in the set of hardcoded primitives.
;;
;; There's a special case optimization we can perform: we can avoid touching
;; the stack for constant arguments; rather than allocate (length (App-operands exp))
;; stack slots, we can do less than that.
;;
;; We have to be sensitive to mutation.
(define (compile-kernel-primitive-application kernel-op exp cenv target linkage)
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
        (make-instruction-sequence
         `(,(make-AssignPrimOpStatement
             target
             (make-CallKernelPrimitiveProcedure 
              kernel-op 
              operand-poss
              expected-operand-types
              typechecks?))))))]
    
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
                    (if (empty? rest-operands)
                        empty-instruction-sequence
                        (make-instruction-sequence `(,(make-PushEnvironment 
                                                       (length rest-operands)
                                                       #f))))]
                   [(stack-popping-code) 
                    (if (empty? rest-operands)
                        empty-instruction-sequence
                        (make-instruction-sequence `(,(make-PopEnvironment 
                                                       (length rest-operands)
                                                       0))))]
                   
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
                                         (compile operand extended-cenv target next-linkage))
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
         stack-popping-code)))]))




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





(: compile-statically-known-lam-application 
   (StaticallyKnownLam App CompileTimeEnvironment Target Linkage 
                       -> InstructionSequence))
(define (compile-statically-known-lam-application static-knowledge exp cenv target linkage)
  (unless (= (length (App-operands exp)) 
             (StaticallyKnownLam-arity static-knowledge))
    (error 'arity-mismatch "~s expected ~s arguments, but received ~s" 
           (StaticallyKnownLam-name static-knowledge)
           (StaticallyKnownLam-arity static-knowledge)
           (length (App-operands exp))))
  
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
                             next-linkage)]
         [operand-codes (map (lambda: ([operand : Expression]
                                       [target : Target])
                                      (compile operand extended-cenv target next-linkage))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda: ([i : Natural])
                                                  (if (< i (sub1 (length (App-operands exp))))
                                                      (make-EnvLexicalReference i #f)
                                                      'val))))])    
    (append-instruction-sequences
     (if (not (empty? (App-operands exp)))
         (make-instruction-sequence `(,(make-PushEnvironment (length (App-operands exp)) #f)))
         empty-instruction-sequence)
     proc-code
     (juggle-operands operand-codes)
     (compile-procedure-call/statically-known-lam static-knowledge 
                                                  cenv
                                                  extended-cenv 
                                                  (length (App-operands exp))
                                                  target
                                                  linkage))))


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



(: compile-general-procedure-call (CompileTimeEnvironment CompileTimeEnvironment
                                                          Natural Target Linkage 
                                                          ->
                                                          InstructionSequence))
;; Assumes the procedure value has been loaded into the proc register.
;; n is the number of arguments passed in.
;; cenv is the compile-time enviroment before arguments have been shifted in.
;; extended-cenv is the compile-time environment after arguments have been shifted in.
(define (compile-general-procedure-call cenv extended-cenv n target linkage)
  (let: ([primitive-branch : LabelLinkage (make-LabelLinkage (make-label 'primitiveBranch))]
         [compiled-branch : LabelLinkage (make-LabelLinkage (make-label 'compiledBranch))]
         [after-call : LabelLinkage (make-LabelLinkage (make-label 'afterCall))])
        (let: ([compiled-linkage : Linkage (if (ReturnLinkage? linkage)
                                               linkage
                                               after-call)])
              (append-instruction-sequences
               (make-instruction-sequence 
                `(,(make-TestAndBranchStatement 'primitive-procedure?
                                                'proc
                                                (LabelLinkage-label primitive-branch))))
               
               
               ;; Compiled branch
               (LabelLinkage-label compiled-branch)
               (make-instruction-sequence
                `(,(make-PerformStatement (make-CheckClosureArity! n))))
               (compile-proc-appl extended-cenv (make-Reg 'val) n target compiled-linkage)
               
               
               
               (LabelLinkage-label primitive-branch)
               (end-with-linkage
                linkage
                cenv
                (append-instruction-sequences
                 (make-instruction-sequence 
                  `(,(make-AssignPrimOpStatement 
                      ;; Optimization: we put the result directly in the registers, or in
                      ;; the appropriate spot on the stack.  This takes into account the popenviroment
                      ;; that happens right afterwards.
                      (adjust-target-depth target n)                     
                      (make-ApplyPrimitiveProcedure n))))
                 (if (not (= n 0))
                     (make-instruction-sequence
                      `(,(make-PopEnvironment n 0)))
                     empty-instruction-sequence)
                 (LabelLinkage-label after-call)))))))


(: compile-procedure-call/statically-known-lam 
   (StaticallyKnownLam CompileTimeEnvironment CompileTimeEnvironment Natural Target Linkage -> InstructionSequence))
(define (compile-procedure-call/statically-known-lam static-knowledge cenv extended-cenv n target linkage)
  (let*: ([after-call : LabelLinkage (make-LabelLinkage (make-label 'afterCall))]
          [compiled-linkage : Linkage (if (ReturnLinkage? linkage)
                                          linkage
                                          after-call)])
         (append-instruction-sequences
          (compile-proc-appl extended-cenv 
                             (make-Label (StaticallyKnownLam-entry-point static-knowledge))
                             n 
                             target
                             compiled-linkage)
          (end-with-linkage
           linkage
           cenv
           (LabelLinkage-label after-call)))))



(: compile-proc-appl (CompileTimeEnvironment (U Label Reg) Natural Target Linkage -> InstructionSequence))
;; Three fundamental cases for general compiled-procedure application.
;;    1.  Tail calls.
;;    2.  Non-tail calls (next/label linkage) that write to val
;;    3.  Calls in argument position (next/label linkage) that write to the stack.
(define (compile-proc-appl cenv-with-args entry-point n target linkage)
  (cond [(ReturnLinkage? linkage)
         (cond
           [(eq? target 'val)
            ;; This case happens when we're in tail position.
            ;; We clean up the stack right before the jump, and do not add
            ;; to the control stack.
            (let: ([num-slots-to-delete : Natural (ensure-natural (- (length cenv-with-args) n))])
                  (append-instruction-sequences
                   (make-instruction-sequence
                    `(,(make-AssignPrimOpStatement 'val 
                                                   (make-GetCompiledProcedureEntry))))
                   (if (> num-slots-to-delete 0)
                       (make-instruction-sequence `(,(make-PopEnvironment num-slots-to-delete n)))
                       empty-instruction-sequence)
                   (make-instruction-sequence
                    `(,(make-GotoStatement entry-point)))))]
           
           [else
            ;; This case should be impossible: return linkage should only
            ;; occur when we're in tail position, and we should be in tail position
            ;; only when the target is the val register.
            (error 'compile "return linkage, target not val: ~s" target)])]
        
        
         [(PromptLinkage? linkage)
          (cond [(eq? target 'val)
                 ;; This case happens for a function call that isn't in
                 ;; tail position.
                 (let ([proc-return (make-label 'procReturn)])
                   (make-instruction-sequence 
                    `(,(make-PushControlFrame proc-return)
                      ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                      ,(make-GotoStatement entry-point)
                      ,proc-return)))]
                
                [else
                 ;; This case happens for evaluating arguments, since the
                 ;; arguments are being installed into the scratch space.
                 (let ([proc-return (make-label 'procReturn)])
                   (make-instruction-sequence
                    `(,(make-PushControlFrame proc-return)
                      ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                      ,(make-GotoStatement entry-point)
                      ,proc-return
                      ,(make-AssignImmediateStatement target (make-Reg 'val)))))])]
        
        [(NextLinkage? linkage)
         (cond [(eq? target 'val)
                ;; This case happens for a function call that isn't in
                ;; tail position.
                (let ([proc-return (make-label 'procReturn)])
                  (make-instruction-sequence 
                   `(,(make-PushControlFrame proc-return)
                     ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                     ,(make-GotoStatement entry-point)
                     ,proc-return)))]
               
               [else
                ;; This case happens for evaluating arguments, since the
                ;; arguments are being installed into the scratch space.
                (let ([proc-return (make-label 'procReturn)])
                  (make-instruction-sequence
                   `(,(make-PushControlFrame proc-return)
                     ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                     ,(make-GotoStatement entry-point)
                     ,proc-return
                     ,(make-AssignImmediateStatement target (make-Reg 'val)))))])]
        
        [(LabelLinkage? linkage)
         (cond [(eq? target 'val)
                ;; This case happens for a function call that isn't in
                ;; tail position.
                (let ([proc-return (make-label 'procReturn)])
                  (make-instruction-sequence 
                   `(,(make-PushControlFrame proc-return)
                     ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                     ,(make-GotoStatement entry-point)
                     ,proc-return
                     ,(make-GotoStatement (make-Label (LabelLinkage-label linkage))))))]
               
               [else
                ;; This case happens for evaluating arguments, since the
                ;; arguments are being installed into the scratch space.
                (let ([proc-return (make-label 'procReturn)])
                  (make-instruction-sequence
                   `(,(make-PushControlFrame proc-return)
                     ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
                     ,(make-GotoStatement entry-point)
                     ,proc-return
                     ,(make-AssignImmediateStatement target (make-Reg 'val))
                     ,(make-GotoStatement (make-Label (LabelLinkage-label linkage))))))])]))



(: extract-static-knowledge (Expression CompileTimeEnvironment ->  
                                        CompileTimeEnvironmentEntry))
;; Statically determines what we know about exp, given the compile time environment.
(define (extract-static-knowledge exp cenv)
  (cond
    [(Lam? exp)
     (make-StaticallyKnownLam (Lam-name exp)
                              (Lam-entry-label exp)
                              (Lam-num-parameters exp))]
    [(and (LocalRef? exp) 
          (not (LocalRef-unbox? exp)))
     (let ([entry (list-ref cenv (LocalRef-depth exp))])
       entry)]
    
    [(ToplevelRef? exp)
     (let: ([name : (U Symbol False ModuleVariable)
                  (list-ref (Prefix-names (ensure-prefix (list-ref cenv (ToplevelRef-depth exp))))
                            (ToplevelRef-pos exp))])
           (cond
             [(ModuleVariable? name)
              name]
             [else
              '?]))]
    
    [(Constant? exp)
     (make-Const (Constant-v exp))]
    
    [else
     '?]))


(: compile-let1 (Let1 CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-let1 exp cenv target linkage)
  (let*: ([rhs-code : InstructionSequence 
                    (compile (Let1-rhs exp)
                             (cons '? cenv)
                             (make-EnvLexicalReference 0 #f)
                             next-linkage)]
          [after-let1 : Symbol (make-label 'afterLetOne)]
          [after-body-code : LabelLinkage (make-LabelLinkage (make-label 'afterLetBody))]
          [extended-cenv : CompileTimeEnvironment (cons (extract-static-knowledge (Let1-rhs exp)
                                                                                  (cons '? cenv))
                                                        cenv)]
          [let-linkage : Linkage
                       (cond
                         [(NextLinkage? linkage)
                          linkage]
                         [(ReturnLinkage? linkage)
                          linkage]
                         [(PromptLinkage? linkage)
                          after-body-code]
                         [(LabelLinkage? linkage)
                          after-body-code])]
          [body-target : Target (adjust-target-depth target 1)]
          [body-code : InstructionSequence
                     (compile (Let1-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences
           (make-instruction-sequence `(,(make-PushEnvironment 1 #f)))
           rhs-code
           body-code
           (LabelLinkage-label after-body-code)
           (make-instruction-sequence `(,(make-PopEnvironment 1 0)))
           after-let1))))



(: compile-let-void (LetVoid CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-let-void exp cenv target linkage)
  (let*: ([n : Natural (LetVoid-count exp)]
          [after-let : Symbol (make-label 'afterLet)]
          [after-body-code : LabelLinkage (make-LabelLinkage (make-label 'afterLetBody))]
          [extended-cenv : CompileTimeEnvironment (append (build-list (LetVoid-count exp) 
                                                                      (lambda: ([i : Natural]) '?))
                                                          cenv)]
          [let-linkage : Linkage
                       (cond
                         [(NextLinkage? linkage)
                          linkage]
                         [(ReturnLinkage? linkage)
                          linkage]
                         [(PromptLinkage? linkage)
                          after-body-code]
                         [(LabelLinkage? linkage)
                          after-body-code])]
          [body-target : Target (adjust-target-depth target n)]
          [body-code : InstructionSequence
                     (compile (LetVoid-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences 
           (if (> n 0)
               (make-instruction-sequence `(,(make-PushEnvironment n (LetVoid-boxes? exp))))
               empty-instruction-sequence)
           body-code
           (LabelLinkage-label after-body-code)
           (if (> n 0)
               (make-instruction-sequence `(,(make-PopEnvironment n 0)))
               empty-instruction-sequence)
           after-let))))



(: compile-let-rec (LetRec CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-let-rec exp cenv target linkage)
  (let*: ([extended-cenv : CompileTimeEnvironment
                         (append (map (lambda: ([p : Lam])
                                               (extract-static-knowledge 
                                                p
                                                (append (build-list (length (LetRec-procs exp))
                                                                    (lambda: ([i : Natural])
                                                                             '?))
                                                        cenv)))
                                      (reverse (LetRec-procs exp)))
                                 cenv)]
          [n : Natural (length (LetRec-procs exp))]
          [after-body-code : LabelLinkage (make-LabelLinkage (make-label 'afterBodyCode))]
          [letrec-linkage : Linkage (cond
                                      [(NextLinkage? linkage)
                                       linkage]
                                      [(ReturnLinkage? linkage)
                                       linkage]
                                      [(PromptLinkage? linkage)
                                       after-body-code]
                                      [(LabelLinkage? linkage)
                                       after-body-code])])
         (end-with-linkage
          linkage
          extended-cenv
          (append-instruction-sequences
           (if (> n 0)
               (make-instruction-sequence `(,(make-PushEnvironment n #f)))
               empty-instruction-sequence)
           
           ;; Install each of the closure shells
           (apply append-instruction-sequences
                  (map (lambda: ([lam : Lam]
                                 [i : Natural])
                                (compile-lambda-shell lam 
                                                      extended-cenv
                                                      (make-EnvLexicalReference i #f) 
                                                      next-linkage))
                       (LetRec-procs exp)
                       (build-list n (lambda: ([i : Natural]) (ensure-natural (- n 1 i))))))
           
           ;; Fix the closure maps of each
           (apply append-instruction-sequences
                  (map (lambda: ([lam : Lam]
                                 [i : Natural])
                                (make-instruction-sequence 
                                 `(,(make-PerformStatement 
                                     (make-FixClosureShellMap! i (Lam-closure-map lam))))))
                       
                       (LetRec-procs exp)
                       (build-list n (lambda: ([i : Natural]) (ensure-natural (- n 1 i))))))
           
           ;; Compile the body
           (compile (LetRec-body exp) extended-cenv (adjust-target-depth target n) letrec-linkage)
           (LabelLinkage-label after-body-code)
           (if (> n 0)
               (make-instruction-sequence `(,(make-PopEnvironment n 0)))
               empty-instruction-sequence)))))



(: compile-install-value (InstallValue CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-install-value exp cenv target linkage)
  (compile (InstallValue-body exp)
           cenv
           (make-EnvLexicalReference (InstallValue-depth exp) (InstallValue-box? exp))
           linkage))



(: compile-box-environment-value (BoxEnv CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-box-environment-value exp cenv target linkage)
  (append-instruction-sequences
   (make-instruction-sequence 
    `(,(make-AssignPrimOpStatement (make-EnvLexicalReference (BoxEnv-depth exp) #f)
                                   (make-MakeBoxedEnvironmentValue (BoxEnv-depth exp)))))
   (compile (BoxEnv-body exp) cenv target linkage)))


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



(: adjust-target-depth (Target Natural -> Target))
(define (adjust-target-depth target n)
  (cond
    [(eq? target 'val)
     target]
    [(eq? target 'proc)
     target]
    [(EnvLexicalReference? target)
     (make-EnvLexicalReference (+ n (EnvLexicalReference-depth target))
                               (EnvLexicalReference-unbox? target))]
    [(EnvPrefixReference? target)
     (make-EnvPrefixReference (+ n (EnvPrefixReference-depth target))
                              (EnvPrefixReference-pos target))]
    [(PrimitivesReference? target)
     target]))


(: adjust-oparg-depth (OpArg Integer -> OpArg))
(define (adjust-oparg-depth arg n)
  (cond
    [(Const? arg)
     arg]
    
    [(Reg? arg)
     arg]
    
    [(Label? arg)
     arg]
    
    [(EnvLexicalReference? arg)
     (make-EnvLexicalReference (ensure-natural (+ n (EnvLexicalReference-depth arg)))
                               (EnvLexicalReference-unbox? arg))]
    [(EnvPrefixReference? arg)
     (make-EnvPrefixReference (ensure-natural (+ n (EnvPrefixReference-depth arg)))
                              (EnvPrefixReference-pos arg))]
    [(EnvWholePrefixReference? arg)
     (make-EnvWholePrefixReference (ensure-natural (+ n (EnvWholePrefixReference-depth arg))))]))




(: adjust-expression-depth (Expression Natural Natural -> Expression))
;; Redirects references to the stack to route around a region of size n.
;; The region begins at offset skip into the environment.
(define (adjust-expression-depth exp n skip)
  (cond
    [(Top? exp)
     (make-Top (Top-prefix exp)
               (adjust-expression-depth (Top-code exp) n (add1 skip)))]
    
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
                           (ToplevelSet-name exp)
                           (adjust-expression-depth (ToplevelSet-value exp) n skip))
         (make-ToplevelSet (ensure-natural (- (ToplevelSet-depth exp) n))
                           (ToplevelSet-pos exp)
                           (ToplevelSet-name exp)
                           (adjust-expression-depth (ToplevelSet-value exp) n skip)))]
    
    [(Branch? exp)
     (make-Branch (adjust-expression-depth (Branch-predicate exp) n skip)
                  (adjust-expression-depth (Branch-consequent exp) n skip)
                  (adjust-expression-depth (Branch-alternative exp) n skip))]
    
    [(Lam? exp)
     (make-Lam (Lam-name exp)
               (Lam-num-parameters exp)
               (Lam-body exp)
               (map (lambda: ([d : Natural]) 
                             (if (< d skip)
                                 d
                                 (ensure-natural (- d n))))
                    (Lam-closure-map exp))
               (Lam-entry-label exp))]
    
    [(Seq? exp)
     (make-Seq (map (lambda: ([action : Expression])
                             (adjust-expression-depth action n skip))
                    (Seq-actions exp)))]

    [(Splice? exp)
     (make-Splice (map (lambda: ([action : Expression])
                                (adjust-expression-depth action n skip))
                       (Splice-actions exp)))]
    
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
                                              (+ skip (length (LetRec-procs exp)))))
                                 (loop (rest procs)))]))
                  (adjust-expression-depth (LetRec-body exp) n 
                                           (+ skip (length (LetRec-procs exp)))))]
    
    [(InstallValue? exp)
     (if (< (InstallValue-depth exp) skip)
         (make-InstallValue (InstallValue-depth exp)
                            (adjust-expression-depth (InstallValue-body exp)
                                                     n
                                                     skip)
                            (InstallValue-box? exp))
         (make-InstallValue (ensure-natural (- (InstallValue-depth exp) n))
                            (adjust-expression-depth (InstallValue-body exp)
                                                     n
                                                     skip)
                            (InstallValue-box? exp)))]
    
    [(BoxEnv? exp)
     (if (< (BoxEnv-depth exp) skip)
         (make-BoxEnv (BoxEnv-depth exp)
                      (adjust-expression-depth (BoxEnv-body exp) n skip))
         (make-BoxEnv (ensure-natural (- (BoxEnv-depth exp) n))
                      (adjust-expression-depth (BoxEnv-body exp) n skip)))]))


