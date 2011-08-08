#lang typed/racket/base
(require "expression-structs.rkt"
         "il-structs.rkt"
         "lexical-structs.rkt"
         (prefix-in ufind: "../union-find.rkt")
         racket/list)
(require/typed "../logger.rkt"
               [log-debug (String -> Void)])
(provide optimize-il)

;; perform optimizations on the intermediate language.
;;



(: optimize-il ((Listof Statement) -> (Listof Statement)))
(define (optimize-il statements)
  ;; For now, replace pairs of PushEnvironment / AssignImmediate(0, ...)
  ;; We should do some more optimizations here, like peephole...
  (let* ([statements (filter not-no-op? statements)]
         [statements (eliminate-no-ops statements)]
         [statements (flatten-adjacent-labels statements)])
    statements))




(: flatten-adjacent-labels ((Listof Statement) -> (Listof Statement)))
;; Squash adjacent labels together.
(define (flatten-adjacent-labels statements)
  (cond
   [(empty? statements)
    empty]
   [else

    ;; The first pass through will collect adjacent labels and equate them.
    (define a-forest (ufind:new-forest))
    (let: loop : 'ok ([stmts : (Listof Statement) (rest statements)]
                      [last-stmt : Statement (first statements)])
      (cond
       [(empty? stmts)
        'ok]
       [else
        (define next-stmt (first stmts))
        (cond
         [(and (symbol? last-stmt) (symbol? next-stmt))
          (log-debug (format "merging label ~a and ~a" last-stmt next-stmt))
          (ufind:union-set a-forest last-stmt next-stmt)
          (loop (rest stmts) next-stmt)]
         
         ;; If there's a label, immediately followed by a direct Goto jump,
         ;; just equate the label and the jump.
         [(and (symbol? last-stmt) (GotoStatement? next-stmt))
          (define goto-target (GotoStatement-target next-stmt))
          (cond
            [(Label? goto-target)
             (log-debug (format "merging label ~a and ~a" last-stmt (Label-name goto-target)))
             (ufind:union-set a-forest last-stmt (Label-name goto-target))
             (loop (rest stmts) next-stmt)]
            [else
             (loop (rest stmts) next-stmt)])]
               
         [else
          (loop (rest stmts) next-stmt)])]))

    
    (: ref (Symbol -> Symbol))
    (define (ref a-label)
      (ufind:find-set a-forest a-label))

    
    (: rewrite-target (Target -> Target))
    (define (rewrite-target target)
      target)

    (: rewrite-oparg (OpArg -> OpArg))
    (define (rewrite-oparg oparg)
      (cond
       [(Const? oparg)
        oparg]
       [(Label? oparg)
        (make-Label (ref (Label-name oparg)))]
       [(Reg? oparg)
        oparg]
       [(EnvLexicalReference? oparg)
        oparg]
       [(EnvPrefixReference? oparg)
        oparg]
       [(EnvWholePrefixReference? oparg)
        oparg]
       [(SubtractArg? oparg)
        oparg]
       [(ControlStackLabel? oparg)
        oparg]
       [(ControlStackLabel/MultipleValueReturn? oparg)
        oparg]
       [(ControlFrameTemporary? oparg)
        oparg]
       [(CompiledProcedureEntry? oparg)
        oparg]
       [(CompiledProcedureClosureReference? oparg)
        oparg]
       [(ModuleEntry? oparg)
        oparg]
       [(IsModuleInvoked? oparg)
        oparg]
       [(IsModuleLinked? oparg)
        oparg]
       [(PrimitiveKernelValue? oparg)
        oparg]
       [(VariableReference? oparg)
        oparg]))


    (: rewrite-primop (PrimitiveOperator -> PrimitiveOperator))
    (define (rewrite-primop op)
      (cond
       [(GetCompiledProcedureEntry? op)
        op]
       [(MakeCompiledProcedure? op)
        (make-MakeCompiledProcedure (ref (MakeCompiledProcedure-label op))
                                    (MakeCompiledProcedure-arity op)
                                    (MakeCompiledProcedure-closed-vals op)
                                    (MakeCompiledProcedure-display-name op))]
       
       [(MakeCompiledProcedureShell? op)
        (make-MakeCompiledProcedureShell (ref (MakeCompiledProcedureShell-label op))
                                         (MakeCompiledProcedureShell-arity op)
                                         (MakeCompiledProcedureShell-display-name op))]
       
       [(ApplyPrimitiveProcedure? op)
        op]
       
       [(MakeBoxedEnvironmentValue? op)
        op]
       
       [(CaptureEnvironment? op)
        op]
       
       [(CaptureControl? op)
        op]
       
       [(CallKernelPrimitiveProcedure? op)
        op]))


    (: rewrite-primcmd (PrimitiveCommand -> PrimitiveCommand))
    (define (rewrite-primcmd cmd)
      (cond
       [(InstallModuleEntry!? cmd)
        (make-InstallModuleEntry! (InstallModuleEntry!-name cmd)
                                  (InstallModuleEntry!-path cmd)
                                  (ref (InstallModuleEntry!-entry-point cmd)))]
       [else
        cmd]
       ;; [(CheckToplevelBound!? cmd)
       ;;  cmd]
       ;; [(CheckClosureArity!? cmd)
       ;;  cmd]
       ;; [(CheckPrimitiveArity!? cmd)
       ;;  cmd]
       
       ;; [(ExtendEnvironment/Prefix!? cmd)
       ;;  cmd]
       ;; [(InstallClosureValues!? cmd)
       ;;  cmd]
       ;; [(FixClosureShellMap!? cmd)
       ;;  cmd]
       
       ;; [(InstallContinuationMarkEntry!? cmd)
       ;;  cmd]
       
       ;; [(SetFrameCallee!? cmd)
       ;;  cmd]
       ;; [(SpliceListIntoStack!? cmd)
       ;;  cmd]
       ;; [(UnspliceRestFromStack!? cmd)
       ;;  cmd]
       
       ;; [(RaiseContextExpectedValuesError!? cmd)
       ;;  cmd]
       ;; [(RaiseArityMismatchError!? cmd)
       ;;  cmd]
       ;; [(RaiseOperatorApplicationError!? cmd)
       ;;  cmd]
       ;; [(RaiseUnimplementedPrimitiveError!? cmd)
       ;;  cmd]
       
       ;; [(RestoreEnvironment!? cmd)
       ;;  cmd]
       ;; [(RestoreControl!? cmd)
       ;;  cmd]
       
       ;; [(MarkModuleInvoked!? cmd)
       ;;  cmd]
       ;; [(AliasModuleAsMain!? cmd)
       ;;  cmd]
       ;; [(FinalizeModuleInvokation!? cmd)
       ;;  cmd]

       ))
    

    (: rewrite-primtest (PrimitiveTest -> PrimitiveTest))
    (define (rewrite-primtest test)
      test)


    
    ;; The second pass will then rewrite references of labels.
    (let: loop : (Listof Statement) ([stmts : (Listof Statement) statements])
      (cond
       [(empty? stmts)
        empty]
       [else
        (define a-stmt (first stmts))
        (cond
         [(symbol? a-stmt)
          (cond
           [(eq? (ref a-stmt) a-stmt)
            (cons (ref a-stmt) (loop (rest stmts)))]
           [else
            (loop (rest stmts))])]

         [(LinkedLabel? a-stmt)
          (cons (make-LinkedLabel (LinkedLabel-label a-stmt)
                                  (ref (LinkedLabel-linked-to a-stmt)))
                (loop (rest stmts)))]
         
         [(DebugPrint? a-stmt)
          (cons a-stmt (loop (rest stmts)))]

         [(Comment? a-stmt)
          (cons a-stmt (loop (rest stmts)))]
         
         [(AssignImmediateStatement? a-stmt)
          (cons (make-AssignImmediateStatement (rewrite-target (AssignImmediateStatement-target a-stmt))
                                               (rewrite-oparg (AssignImmediateStatement-value a-stmt)))
                (loop (rest stmts)))]

         [(AssignPrimOpStatement? a-stmt)
          (cons (make-AssignPrimOpStatement (rewrite-target (AssignPrimOpStatement-target a-stmt))
                                            (rewrite-primop (AssignPrimOpStatement-op a-stmt)))
                (loop (rest stmts)))]

         [(PerformStatement? a-stmt)
          (cons (make-PerformStatement (rewrite-primcmd (PerformStatement-op a-stmt)))
                (loop (rest stmts)))]
         
         [(PopEnvironment? a-stmt)
          (cons (make-PopEnvironment (rewrite-oparg (PopEnvironment-n a-stmt))
                                     (rewrite-oparg (PopEnvironment-skip a-stmt)))
                (loop (rest stmts)))]

         [(PushEnvironment? a-stmt)
          (cons a-stmt (loop (rest stmts)))]
         
         [(PushImmediateOntoEnvironment? a-stmt)
          (cons (make-PushImmediateOntoEnvironment (rewrite-oparg (PushImmediateOntoEnvironment-value a-stmt))
                                                   (PushImmediateOntoEnvironment-box? a-stmt))
                (loop (rest stmts)))]
         
         [(PushControlFrame/Generic? a-stmt)
          (cons a-stmt (loop (rest stmts)))]

         [(PushControlFrame/Call? a-stmt)
          (define a-label (PushControlFrame/Call-label a-stmt))
          (cons (make-PushControlFrame/Call
                 (make-LinkedLabel (LinkedLabel-label a-label)
                                   (ref (LinkedLabel-linked-to a-label))))
                (loop (rest stmts)))]
         
         [(PushControlFrame/Prompt? a-stmt)
          (define a-label (PushControlFrame/Prompt-label a-stmt))
          (cons (make-PushControlFrame/Prompt (let ([tag (PushControlFrame/Prompt-tag a-stmt)])
                                                (if (DefaultContinuationPromptTag? tag)
                                                    tag
                                                    (rewrite-oparg tag)))
                                              (make-LinkedLabel (LinkedLabel-label a-label)
                                                                (ref (LinkedLabel-linked-to a-label))))
                (loop (rest stmts)))]
         
         [(PopControlFrame? a-stmt)
          (cons a-stmt (loop (rest stmts)))]

         [(GotoStatement? a-stmt)
          (define target (GotoStatement-target a-stmt))
          (cond
           [(Label? target)
            (cons (make-GotoStatement (make-Label (ref (Label-name target))))
                  (loop (rest stmts)))]
           [else
            (cons a-stmt (loop (rest stmts)))])]


         [(TestAndJumpStatement? a-stmt)
          (cons (make-TestAndJumpStatement (rewrite-primtest (TestAndJumpStatement-op a-stmt))
                                           (ref (TestAndJumpStatement-label a-stmt)))
                (loop (rest stmts)))])]))]))
    

    


(: eliminate-no-ops ((Listof Statement) -> (Listof Statement)))
(define (eliminate-no-ops statements)
  (let loop ([statements statements])
      (cond
       [(empty? statements)
        empty]
       [else
        (let ([first-stmt (first statements)])
          (: default (-> (Listof Statement)))
          (define (default)
            (cons first-stmt (loop (rest statements))))
          (cond
           [(empty? (rest statements))
            (default)]
           [else
            (let ([second-stmt (second statements)])
              (cond
               [(and (PushEnvironment? first-stmt)
                     (equal? first-stmt (make-PushEnvironment 1 #f))
                     (AssignImmediateStatement? second-stmt))
                (let ([target (AssignImmediateStatement-target second-stmt)])
                  (cond
                   [(equal? target (make-EnvLexicalReference 0 #f))
                    (cons (make-PushImmediateOntoEnvironment 
                           (adjust-oparg-depth 
                            (AssignImmediateStatement-value second-stmt) -1)
                           #f)
                          (loop (rest (rest statements))))]
                   [else
                    (default)]))]
               [else
                (default)]))]))])))


(: not-no-op? (Statement -> Boolean))
(define (not-no-op? stmt) (not (no-op? stmt)))


(: no-op? (Statement -> Boolean))
;; Produces true if the statement should have no effect.
(define (no-op? stmt)
  (cond
    [(symbol? stmt)
     #f]
    
    [(LinkedLabel? stmt)
     #f]
    
    [(DebugPrint? stmt)
     #f]
    
    [(AssignImmediateStatement? stmt)
     (equal? (AssignImmediateStatement-target stmt)
             (AssignImmediateStatement-value stmt))]

    [(AssignPrimOpStatement? stmt)
     #f]
    
    [(PerformStatement? stmt)
     #f]
    
    [(GotoStatement? stmt)
     #f]
    
    [(TestAndJumpStatement? stmt)
     #f]
    
    [(PopEnvironment? stmt)
     (and (Const? (PopEnvironment-n stmt))
          (equal? (PopEnvironment-n stmt) 
                  (make-Const 0)))]
     
    [(PushEnvironment? stmt)
     (= (PushEnvironment-n stmt) 0)]
    
    [(PushImmediateOntoEnvironment? stmt)
     #f]

    [(PushControlFrame/Generic? stmt)
     #f]
    
    [(PushControlFrame/Call? stmt)
     #f]
    
    [(PushControlFrame/Prompt? stmt)
     #f]
    
    [(PopControlFrame? stmt)
     #f]
    [(Comment? stmt)
     #t]))






(: adjust-oparg-depth (OpArg Integer -> OpArg))
(define (adjust-oparg-depth oparg n)
  (cond
    [(Const? oparg) oparg]
    [(Label? oparg) oparg]
    [(Reg? oparg) oparg]
    [(EnvLexicalReference? oparg)
     (make-EnvLexicalReference (ensure-natural (+ n (EnvLexicalReference-depth oparg)))
                               (EnvLexicalReference-unbox? oparg))]
    [(EnvPrefixReference? oparg)
     (make-EnvPrefixReference (ensure-natural (+ n (EnvPrefixReference-depth oparg)))
                              (EnvPrefixReference-pos oparg))]
    [(EnvWholePrefixReference? oparg)
     (make-EnvWholePrefixReference (ensure-natural (+ n (EnvWholePrefixReference-depth oparg))))]
    [(SubtractArg? oparg)
     (make-SubtractArg (adjust-oparg-depth (SubtractArg-lhs oparg) n)
                       (adjust-oparg-depth (SubtractArg-rhs oparg) n))]
    [(ControlStackLabel? oparg)
     oparg]
    [(ControlStackLabel/MultipleValueReturn? oparg)
     oparg]
    [(ControlFrameTemporary? oparg)
     oparg]
    [(CompiledProcedureEntry? oparg)
     (make-CompiledProcedureEntry (adjust-oparg-depth (CompiledProcedureEntry-proc oparg) n))]
    [(CompiledProcedureClosureReference? oparg)
     (make-CompiledProcedureClosureReference 
      (adjust-oparg-depth (CompiledProcedureClosureReference-proc oparg) n)
      (CompiledProcedureClosureReference-n oparg))]
    [(PrimitiveKernelValue? oparg)
     oparg]
    [(ModuleEntry? oparg)
     oparg]
    [(IsModuleInvoked? oparg)
     oparg]
    [(IsModuleLinked? oparg)
     oparg]
    [(VariableReference? oparg)
     (let ([t (VariableReference-toplevel oparg)])
       (make-VariableReference 
        (make-ToplevelRef (ensure-natural (+ n (ToplevelRef-depth t)))
                          (ToplevelRef-pos t)
                          (ToplevelRef-constant? t)
                          (ToplevelRef-check-defined? t))))]))


(define-predicate natural? Natural)
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))