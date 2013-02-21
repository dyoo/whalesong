#lang typed/racket/base
(require "../compiler/expression-structs.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../helpers.rkt"
         "../parameters.rkt"
         racket/list)

(provide collect-general-jump-targets
         collect-entry-points)



(: collect-general-jump-targets ((Listof Statement) -> (Listof Symbol)))
;; collects all the labels that are potential targets for GOTOs or branches.
(define (collect-general-jump-targets stmts)
  
  (: collect-statement (Statement -> (Listof Symbol)))
  (define (collect-statement stmt)
    (cond
      [(symbol? stmt)
       empty]
      [(LinkedLabel? stmt)
       (list (LinkedLabel-label stmt)
             (LinkedLabel-linked-to stmt))]
      [(DebugPrint? stmt)
       empty]
      [(AssignImmediate? stmt)
       (let: ([v : OpArg (AssignImmediate-value stmt)])
         (collect-input v))]
      [(AssignPrimOp? stmt)
       (collect-primitive-operator (AssignPrimOp-op stmt))]
      [(Perform? stmt)
       (collect-primitive-command (Perform-op stmt))]
      [(TestAndJump? stmt)
       (list (TestAndJump-label stmt))]
      [(Goto? stmt)
       (collect-input (Goto-target stmt))]
      [(PushEnvironment? stmt)
       empty]
      [(PopEnvironment? stmt)
       empty]
      [(PushImmediateOntoEnvironment? stmt)
       (collect-input (PushImmediateOntoEnvironment-value stmt))]
      [(PushControlFrame/Generic? stmt)
       empty]
      [(PushControlFrame/Call? stmt)
       (label->labels (PushControlFrame/Call-label stmt))]
      [(PushControlFrame/Prompt? stmt)
       (label->labels (PushControlFrame/Prompt-label stmt))]
      [(PopControlFrame? stmt)
       empty]
      [(Comment? stmt)
       empty]))
  
  
  
  (: collect-input (OpArg -> (Listof Symbol)))
  (define (collect-input an-input)
    (cond
      [(Reg? an-input)
       empty]
      [(Const? an-input)
       empty]
      [(Label? an-input)
       (list (Label-name an-input))]
      [(EnvLexicalReference? an-input)
       empty]
      [(EnvPrefixReference? an-input)
       empty]
      [(EnvWholePrefixReference? an-input)
       empty]
      [(SubtractArg? an-input)
       (append (collect-input (SubtractArg-lhs an-input))
               (collect-input (SubtractArg-rhs an-input)))]
      [(ControlStackLabel? an-input)
       empty]
      [(ControlStackLabel/MultipleValueReturn? an-input)
       empty]
      [(ControlFrameTemporary? an-input)
       empty]
      [(CompiledProcedureEntry? an-input)
       (collect-input (CompiledProcedureEntry-proc an-input))]
      [(CompiledProcedureClosureReference? an-input)
       (collect-input (CompiledProcedureClosureReference-proc an-input))]
      [(PrimitiveKernelValue? an-input)
       empty]
      [(ModuleEntry? an-input)
       empty]
      [(ModulePredicate? an-input)
       empty]
      [(VariableReference? an-input)
       empty]))
  
  
  (: collect-location ((U Reg Label) -> (Listof Symbol)))
  (define (collect-location a-location)
    (cond
      [(Reg? a-location)
       empty]
      [(Label? a-location)
       (list (Label-name a-location))]))
  
  (: collect-primitive-operator (PrimitiveOperator -> (Listof Symbol)))
  (define (collect-primitive-operator op)
    (cond
      [(GetCompiledProcedureEntry? op)
       empty]
      [(MakeCompiledProcedure? op)
       (list (MakeCompiledProcedure-label op))]
      [(MakeCompiledProcedureShell? op)
       (list (MakeCompiledProcedureShell-label op))]
      [(ApplyPrimitiveProcedure? op)
        empty]
      [(CaptureEnvironment? op)
       empty]
      [(CaptureControl? op)
       empty]
      [(MakeBoxedEnvironmentValue? op)
       empty]
      [(CallKernelPrimitiveProcedure? op)
       empty]
      [(ModuleVariable? op)
       empty]
      [(PrimitivesReference? op)
       empty]))

  
  (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
  (define (collect-primitive-command op)
    (cond
      [(InstallModuleEntry!? op)
       (list (InstallModuleEntry!-entry-point op))]
      [else
       empty]))
  
  (: start-time Real)
  (define start-time (current-inexact-milliseconds))

  (: result (Listof Symbol))
  (define result
    (unique/eq?
     (let: loop : (Listof Symbol) ([stmts : (Listof Statement) stmts])
       (cond [(empty? stmts)
              empty]
             [else
              (let: ([stmt : Statement (first stmts)])
                (append (collect-statement stmt)
                        (loop (rest stmts))))]))))

  (: end-time Real)
  (define end-time (current-inexact-milliseconds))
  (fprintf (current-timing-port) "  collect-general-jump-targets: ~a milliseconds\n" (- end-time start-time))
  result)







(: collect-entry-points ((Listof Statement) -> (Listof Symbol)))
;; collects all the labels that are general entry points.  The entry points are
;; from the starting basic block, from functions headers, and finally return points.
(define (collect-entry-points stmts)
  
  (: collect-statement (Statement -> (Listof Symbol)))
  (define (collect-statement stmt)
    (cond
      [(symbol? stmt)
       empty]
      [(LinkedLabel? stmt)
       (list (LinkedLabel-label stmt)
             (LinkedLabel-linked-to stmt))]
      [(DebugPrint? stmt)
       empty]
      [(AssignImmediate? stmt)
       (let: ([v : OpArg (AssignImmediate-value stmt)])
         (collect-input v))]
      [(AssignPrimOp? stmt)
       (collect-primitive-operator (AssignPrimOp-op stmt))]
      [(Perform? stmt)
       (collect-primitive-command (Perform-op stmt))]
      [(TestAndJump? stmt)
       empty]
      [(Goto? stmt)
       empty]
      [(PushEnvironment? stmt)
       empty]
      [(PopEnvironment? stmt)
       empty]
      [(PushImmediateOntoEnvironment? stmt)
       (collect-input (PushImmediateOntoEnvironment-value stmt))]
      [(PushControlFrame/Generic? stmt)
       empty]
      [(PushControlFrame/Call? stmt)
       (label->labels (PushControlFrame/Call-label stmt))]
      [(PushControlFrame/Prompt? stmt)
       (label->labels (PushControlFrame/Prompt-label stmt))]
      [(PopControlFrame? stmt)
       empty]
      [(Comment? stmt)
       empty]))
  
  
  
  (: collect-input (OpArg -> (Listof Symbol)))
  (define (collect-input an-input)
    (cond
      [(Reg? an-input)
       empty]
      [(Const? an-input)
       empty]
      [(Label? an-input)
       (list (Label-name an-input))]
      [(EnvLexicalReference? an-input)
       empty]
      [(EnvPrefixReference? an-input)
       empty]
      [(EnvWholePrefixReference? an-input)
       empty]
      [(SubtractArg? an-input)
       (append (collect-input (SubtractArg-lhs an-input))
               (collect-input (SubtractArg-rhs an-input)))]
      [(ControlStackLabel? an-input)
       empty]
      [(ControlStackLabel/MultipleValueReturn? an-input)
       empty]
      [(ControlFrameTemporary? an-input)
       empty]
      [(CompiledProcedureEntry? an-input)
       (collect-input (CompiledProcedureEntry-proc an-input))]
      [(CompiledProcedureClosureReference? an-input)
       (collect-input (CompiledProcedureClosureReference-proc an-input))]
      [(PrimitiveKernelValue? an-input)
       empty]
      [(ModuleEntry? an-input)
       empty]
      [(ModulePredicate? an-input)
       empty]
      [(VariableReference? an-input)
       empty]))
  
  
  (: collect-location ((U Reg Label) -> (Listof Symbol)))
  (define (collect-location a-location)
    (cond
      [(Reg? a-location)
       empty]
      [(Label? a-location)
       (list (Label-name a-location))]))
  
  (: collect-primitive-operator (PrimitiveOperator -> (Listof Symbol)))
  (define (collect-primitive-operator op)
    (cond
      [(GetCompiledProcedureEntry? op)
       empty]
      [(MakeCompiledProcedure? op)
       (list (MakeCompiledProcedure-label op))]
      [(MakeCompiledProcedureShell? op)
       (list (MakeCompiledProcedureShell-label op))]
      [(ApplyPrimitiveProcedure? op)
        empty]
      [(CaptureEnvironment? op)
       empty]
      [(CaptureControl? op)
       empty]
      [(MakeBoxedEnvironmentValue? op)
       empty]
      [(CallKernelPrimitiveProcedure? op)
       empty]
      [(ModuleVariable? op)
       empty]
      [(PrimitivesReference? op)
       empty]))
  
  (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
  (define (collect-primitive-command op)
    (cond
      [(InstallModuleEntry!? op)
       (list (InstallModuleEntry!-entry-point op))]
      [else
       empty]
      ;; currently written this way because I'm hitting some bad type-checking behavior.
      #;([(CheckToplevelBound!? op)
          empty]
         [(CheckClosureAndArity!? op)
          empty]
         [(CheckPrimitiveArity!? op)
          empty]
         [(ExtendEnvironment/Prefix!? op)
          empty]
         [(InstallClosureValues!? op)
          empty]
         [(RestoreEnvironment!? op)
          empty]
         [(RestoreControl!? op)
          empty]
         [(SetFrameCallee!? op)
          empty]
         [(SpliceListIntoStack!? op)
          empty]
         [(UnspliceRestFromStack!? op)
          empty]
         [(FixClosureShellMap!? op)
          empty]
         [(InstallContinuationMarkEntry!? op)
          empty]
         [(RaiseContextExpectedValuesError!? op)
          empty]
         [(RaiseArityMismatchError!? op)
          empty]
         [(RaiseOperatorApplicationError!? op)
          empty])))
  
  
  (unique/eq?
   (let: loop : (Listof Symbol) ([stmts : (Listof Statement) stmts])
     (cond [(empty? stmts)
            empty]
           [else
            (let: ([stmt : Statement (first stmts)])
              (append (collect-statement stmt)
                      (loop (rest stmts))))]))))







(: label->labels ((U Symbol LinkedLabel) -> (Listof Symbol)))
(define (label->labels label)
  (cond
    [(symbol? label)
     (list label)]
    [(LinkedLabel? label)
     (list (LinkedLabel-label label)
           (LinkedLabel-linked-to label))]))

