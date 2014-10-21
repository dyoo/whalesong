#lang whalesong (require "../selfhost-lang.rkt")
(require "expression-structs.rkt"
         "il-structs.rkt"
         "lexical-structs.rkt"
         (prefix-in ufind: "../union-find.rkt")
         racket/list)
; (require/typed "../logger.rkt" [log-debug (String -> Void)]) ; TODO /soegaard
(provide optimize-il)

;; perform optimizations on the intermediate language.
;;



(: optimize-il ((Listof Statement) -> (Listof Statement)))
(define (optimize-il statements)
  ;; For now, replace pairs of PushEnvironment / AssignImmediate(0, ...)
  ;; We should do some more optimizations here, like peephole...
  (let* ([statements (filter not-no-op? statements)]
         [statements (pairwise-reductions statements)]
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
    (let loop ; : 'ok 
      ([stmts (rest statements)]       ; : (Listof Statement) 
       [last-stmt (first statements)]) ; : Statement 
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
         [(and (symbol? last-stmt) (Goto? next-stmt))
          (define goto-target (Goto-target next-stmt))
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
       [(ModulePredicate? oparg)
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
       
       
       [(MakeBoxedEnvironmentValue? op)
        op]
       
       [(CaptureEnvironment? op)
        op]
       
       [(CaptureControl? op)
        op]
       
       [(CallKernelPrimitiveProcedure? op)
        op]
       
       [(ApplyPrimitiveProcedure? op)
        op]

       [(ModuleVariable? op)
        op]
       
       [(PrimitivesReference? op)
        op]
       
       [(GlobalsReference? op)
        op]))


    (: rewrite-primcmd (PrimitiveCommand -> PrimitiveCommand))
    (define (rewrite-primcmd cmd)
      (cond
       [(InstallModuleEntry!? cmd)
        (make-InstallModuleEntry! (InstallModuleEntry!-name cmd)
                                  (InstallModuleEntry!-path cmd)
                                  (ref (InstallModuleEntry!-entry-point cmd)))]
       [else
        cmd]))
    

    (: rewrite-primtest (PrimitiveTest -> PrimitiveTest))
    (define (rewrite-primtest test)
      test)


    
    ;; The second pass will then rewrite references of labels.
    (let loop ; : (Listof Statement) 
      ([stmts statements]) ; : (Listof Statement) 
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
          (cons a-stmt (loop (rest stmts)))
          #;(loop (rest stmts))
          ]

         [(Comment? a-stmt)
          ;(loop (rest stmts))
          (cons a-stmt (loop (rest stmts)))
          ]

         [(MarkEntryPoint? a-stmt)
          (cons a-stmt (loop (rest stmts)))]
         
         [(AssignImmediate? a-stmt)
          (cons (make-AssignImmediate (rewrite-target (AssignImmediate-target a-stmt))
                                               (rewrite-oparg (AssignImmediate-value a-stmt)))
                (loop (rest stmts)))]

         [(AssignPrimOp? a-stmt)
          (cons (make-AssignPrimOp (rewrite-target (AssignPrimOp-target a-stmt))
                                            (rewrite-primop (AssignPrimOp-op a-stmt)))
                (loop (rest stmts)))]

         [(Perform? a-stmt)
          (cons (make-Perform (rewrite-primcmd (Perform-op a-stmt)))
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

         [(Goto? a-stmt)
          (define target (Goto-target a-stmt))
          (cond
           [(Label? target)
            (cons (make-Goto (make-Label (ref (Label-name target))))
                  (loop (rest stmts)))]
           [else
            (cons a-stmt (loop (rest stmts)))])]


         [(TestAndJump? a-stmt)
          (cons (make-TestAndJump (rewrite-primtest (TestAndJump-op a-stmt))
                                           (ref (TestAndJump-label a-stmt)))
                (loop (rest stmts)))])]))]))
    

    


(: pairwise-reductions ((Listof Statement) -> (Listof Statement)))
(define (pairwise-reductions statements)
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
               
               ;; A PushEnvironment followed by a direct AssignImmediate can be reduced to a single
               ;; instruction.
               [(and (PushEnvironment? first-stmt)
                     (equal? first-stmt (make-PushEnvironment 1 #f))
                     (AssignImmediate? second-stmt))
                (let ([target (AssignImmediate-target second-stmt)])
                  (cond
                   [(equal? target (make-EnvLexicalReference 0 #f))
                    (loop (cons (make-PushImmediateOntoEnvironment 
                                 (adjust-oparg-depth 
                                  (AssignImmediate-value second-stmt) -1)
                                 #f)
                                (rest (rest statements))))]
                   [else
                    (default)]))]

               ;; Adjacent PopEnvironments with constants can be reduced to single ones
               [(and (PopEnvironment? first-stmt)
                     (PopEnvironment? second-stmt))
                (let ([first-n (PopEnvironment-n first-stmt)]
                      [second-n (PopEnvironment-n second-stmt)]
                      [first-skip (PopEnvironment-skip first-stmt)]
                      [second-skip (PopEnvironment-skip second-stmt)])
                  (cond [(and (Const? first-n) (Const? second-n) (Const? first-skip) (Const? second-skip))
                         (let ([first-n-val (Const-const first-n)]
                               [second-n-val (Const-const second-n)]
                               [first-skip-val (Const-const first-skip)]
                               [second-skip-val (Const-const second-skip)])
                           (cond
                            [(and (number? first-n-val)
                                  (number? second-n-val)
                                  (number? first-skip-val) (= first-skip-val 0)
                                  (number? second-skip-val) (= second-skip-val 0))
                             (loop (cons (make-PopEnvironment (make-Const (+ first-n-val second-n-val))
                                                              (make-Const 0))
                                         (rest (rest statements))))]
                            [else
                             (default)]))]
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
     #f
     #;#t]
    
    [(MarkEntryPoint? stmt)
     #f]
    
    [(AssignImmediate? stmt)
     (equal? (AssignImmediate-target stmt)
             (AssignImmediate-value stmt))]

    [(AssignPrimOp? stmt)
     #f]
    
    [(Perform? stmt)
     #f]
    
    [(Goto? stmt)
     #f]
    
    [(TestAndJump? stmt)
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
     #f]))






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
                              (EnvPrefixReference-pos oparg)
                              (EnvPrefixReference-modvar? oparg))]
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
    [(ModulePredicate? oparg)
     oparg]
    [(VariableReference? oparg)
     (let ([t (VariableReference-toplevel oparg)])
       (make-VariableReference 
        (make-ToplevelRef (ensure-natural (+ n (ToplevelRef-depth t)))
                          (ToplevelRef-pos t)
                          (ToplevelRef-constant? t)
                          (ToplevelRef-check-defined? t))))]))


; (define-predicate natural? Natural)
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))
