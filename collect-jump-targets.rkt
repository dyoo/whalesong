#lang typed/racket/base
(require "il-structs.rkt"
	 "lexical-structs.rkt"
	 "helpers.rkt"
	 racket/list)

(provide collect-general-jump-targets)




(: collect-general-jump-targets ((Listof Statement) -> (Listof Symbol)))
;; collects all the labels that are potential targets for GOTOs or branches.
(define (collect-general-jump-targets stmts)
  (unique/eq?
   (let: loop : (Listof Symbol) ([stmts : (Listof Statement) stmts])
         (cond [(empty? stmts)
                empty]
               [else
                (let: ([stmt : Statement (first stmts)])
		      (append (collect-statement stmt)
			      (loop (rest stmts))))]))))





(: collect-statement (Statement -> (Listof Symbol)))
(define (collect-statement stmt)
  (cond
   [(symbol? stmt)
    empty]
   [(LinkedLabel? stmt)
    (list (LinkedLabel-label stmt)
	  (LinkedLabel-linked-to stmt))]
   [(AssignImmediateStatement? stmt)
    (let: ([v : OpArg (AssignImmediateStatement-value stmt)])
	  (collect-input v))]
   [(AssignPrimOpStatement? stmt)
    (collect-primitive-operator (AssignPrimOpStatement-op stmt))]
   [(PerformStatement? stmt)
    (collect-primitive-command (PerformStatement-op stmt))]
   [(TestAndBranchStatement? stmt)
    (list (TestAndBranchStatement-label stmt))]
   [(GotoStatement? stmt)
    (collect-input (GotoStatement-target stmt))]
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
   [(CompiledProcedureEntry? an-input)
    (collect-input (CompiledProcedureEntry-proc an-input))]
   [(ControlFrameTemporary? an-input)
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
    empty]))

(: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
(define (collect-primitive-command op)
  (cond
   [(CheckToplevelBound!? op)
    empty]
   [(CheckClosureArity!? op)
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
    empty]))





(: label->labels ((U Symbol LinkedLabel) -> (Listof Symbol)))
(define (label->labels label)
  (cond
   [(symbol? label)
    (list label)]
   [(LinkedLabel? label)
    (list (LinkedLabel-label label)
	  (LinkedLabel-linked-to label))]))

