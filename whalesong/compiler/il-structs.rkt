#lang typed/racket/base
(provide (all-defined-out))

(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "kernel-primitives.rkt"
         "arity-structs.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registers of the machine:

(define-type StackRegisterSymbol (U 'control 'env))
(define-type AtomicRegisterSymbol (U 'val 'proc 'argcount))
(define-type RegisterSymbol (U StackRegisterSymbol AtomicRegisterSymbol))

(define-predicate AtomicRegisterSymbol? AtomicRegisterSymbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; An operation can refer to the following arguments:
(define-type OpArg (U Const ;; an constant
                      Label ;; an label
                      Reg   ;; an register 
                      EnvLexicalReference ;; a reference into the stack
                      EnvPrefixReference  ;; a reference into an element in the toplevel.
                      EnvWholePrefixReference ;; a reference into a toplevel prefix in the stack.
                      SubtractArg
		      ControlStackLabel
		      ControlStackLabel/MultipleValueReturn
                      ControlFrameTemporary
                      CompiledProcedureEntry
                      CompiledProcedureClosureReference
                      ModuleEntry
                      ModulePredicate
                      PrimitiveKernelValue
                      VariableReference
                      ))


;; Targets: these are the allowable lhs's for a targetted assignment.
(define-type Target (U AtomicRegisterSymbol 
                       EnvLexicalReference
                       EnvPrefixReference
                       PrimitivesReference                   
                       ControlFrameTemporary
                       ModulePrefixTarget
                       ))

(define-struct: ModuleVariableThing () #:transparent)


;; When we need to store a value temporarily in the top control frame, we can use this as a target.
(define-struct: ControlFrameTemporary ([name : (U 'pendingContinuationMarkKey ;; for continuation marks
                                                  'pendingApplyValuesProc ;; for apply-values
                                                  'pendingBegin0Count
                                                  'pendingBegin0Values
                                                  )])
  #:transparent)


;; Targetting the prefix attribute of a module.
(define-struct: ModulePrefixTarget ([path : ModuleLocator])
  #:transparent)

(define-struct: ModuleVariableReference ([name : Symbol]
                                         [module-name : ModuleLocator])
  #:transparent)



(define-type const-value
  (Rec C
       (U Symbol
          String
          Number
          Boolean
          Void
          Null
          Char
          Bytes
          Path
          (Pairof C C)
          (Vectorof C)
          (Boxof C))))


(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : AtomicRegisterSymbol])
  #:transparent)
(define-struct: Const ([const : const-value])
  #:transparent)

;; Limited arithmetic on OpArgs
(define-struct: SubtractArg ([lhs : OpArg]
                             [rhs : OpArg])
  #:transparent)


(: new-SubtractArg (OpArg OpArg -> OpArg))
(define (new-SubtractArg lhs rhs)
  ;; FIXME: do some limited constant folding here
  (cond
   [(and (Const? lhs)(Const? rhs))
    (let ([lhs-val (Const-const lhs)]
          [rhs-val (Const-const rhs)])
      (cond [(and (number? lhs-val)
                  (number? rhs-val))
             (make-Const (- lhs-val rhs-val))]
            [else
             (make-SubtractArg lhs rhs)]))]
   [(Const? rhs)
    (let ([rhs-val (Const-const rhs)])
      (cond
       [(and (number? rhs-val)
             (= rhs-val 0))
        lhs]
       [else
        (make-SubtractArg lhs rhs)]))]
   [else
    (make-SubtractArg lhs rhs)]))






;; Gets the return address embedded at the top of the control stack.
(define-struct: ControlStackLabel ()
  #:transparent)

;; Gets the secondary (mulitple-value-return) return address embedded
;; at the top of the control stack.
(define-struct: ControlStackLabel/MultipleValueReturn ()
  #:transparent)

;; Get the entry point of a compiled procedure.
(define-struct: CompiledProcedureEntry ([proc : OpArg])
  #:transparent)


;; Get at the nth value in a closure's list of closed values.
(define-struct: CompiledProcedureClosureReference ([proc : OpArg]
                                                   [n : Natural])
  #:transparent)


(define-struct: PrimitivesReference ([name : Symbol])
  #:transparent)

;; Produces the entry point of the module.
(define-struct: ModuleEntry ([name : ModuleLocator])
  #:transparent)


(define-struct: ModulePredicate ([module-name : ModuleLocator]
                                 [pred : (U 'invoked? 'linked?)])
  #:transparent)



;; A straight-line statement includes non-branching stuff.
(define-type StraightLineStatement (U
                                    DebugPrint
                                    Comment
                                    
                                    AssignImmediate
                                    AssignPrimOp
                                    Perform
                                    
                                    PopEnvironment
                                    PushEnvironment
                                    PushImmediateOntoEnvironment
                                    
                                    PushControlFrame/Generic
                                    PushControlFrame/Call
                                    PushControlFrame/Prompt
                                    PopControlFrame))

(define-type BranchingStatement (U Goto TestAndJump))
                                 

;; instruction sequences
(define-type UnlabeledStatement (U StraightLineStatement BranchingStatement))

(define-predicate UnlabeledStatement? UnlabeledStatement)


;; Debug print statement.
(define-struct: DebugPrint ([value : OpArg])
  #:transparent)


(define-type Statement (U UnlabeledStatement
                          Symbol      ;; label
                          LinkedLabel ;; Label with a reference to a multiple-return-value label
                          ))


(define-struct: LinkedLabel ([label : Symbol]
                             [linked-to : Symbol])
  #:transparent)


;; Returns a pair of labels, the first being the mutiple-value-return
;; label and the second its complementary single-value-return label.
(: new-linked-labels (Symbol -> (Values Symbol LinkedLabel)))
(define (new-linked-labels sym)
  (define a-label-multiple (make-label (string->symbol (format "~aMultiple" sym))))
  (define a-label (make-LinkedLabel (make-label sym) a-label-multiple))
  (values a-label-multiple a-label))





;; FIXME: it would be nice if I can reduce AssignImmediate and
;; AssignPrimOp into a single Assign statement, but I run into major
;; issues with Typed Racket taking minutes to compile.  So we're
;; running into some kind of degenerate behavior.
(define-struct: AssignImmediate ([target : Target]
                                 [value : OpArg])
  #:transparent)
(define-struct: AssignPrimOp ([target : Target]
                              [op : PrimitiveOperator])
  #:transparent)


;; Pop n slots from the environment, skipping past a few first.
(define-struct: PopEnvironment ([n : OpArg]
                                [skip : OpArg])
  #:transparent)
(define-struct: PushEnvironment ([n : Natural]
                                 [unbox? : Boolean])
  #:transparent)


;; Evaluate the value, and then push it onto the top of the environment.
(define-struct: PushImmediateOntoEnvironment ([value : OpArg]
                                              [box? : Boolean])
  #:transparent)


(define-struct: PopControlFrame () 
  #:transparent)


;; A generic control frame only holds marks and other temporary variables.
(define-struct: PushControlFrame/Generic ()
  #:transparent)

;; Adding a frame for getting back after procedure application.
;; The 'proc register must hold either #f or a closure at the time of
;; this call, as the control frame will hold onto the called procedure record.
(define-struct: PushControlFrame/Call ([label : LinkedLabel]) 
  #:transparent)

(define-struct: PushControlFrame/Prompt ([tag : (U OpArg DefaultContinuationPromptTag)]
                                         [label : LinkedLabel]
                                         ;; TODO: add handler and arguments
                                         )
  #:transparent)

(define-struct: DefaultContinuationPromptTag ()
  #:transparent)
(define default-continuation-prompt-tag 
  (make-DefaultContinuationPromptTag))




(define-struct: Goto ([target : (U Label 
                                            Reg
                                            ModuleEntry
                                            CompiledProcedureEntry)]) 
  #:transparent)

(define-struct: Perform ([op : PrimitiveCommand])
  #:transparent)



(define-struct: TestAndJump ([op : PrimitiveTest]
                                      [label : Symbol])
  #:transparent)


(define-struct: Comment ([val : Any])
  #:transparent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Operators

;; The operators that return values, that are used in AssignPrimopStatement.
;; The reason this is here is really to get around what looks like a Typed Racket issue.
;; I would prefer to move these all to OpArgs, but if I do, Typed Racket takes much longer
;; to type my program than I'd like.
(define-type PrimitiveOperator (U GetCompiledProcedureEntry
                                  MakeCompiledProcedure
                                  MakeCompiledProcedureShell

                                  ModuleVariable
                                  PrimitivesReference
                                  
                                  MakeBoxedEnvironmentValue

                                  CaptureEnvironment
                                  CaptureControl
                                  
                                  CallKernelPrimitiveProcedure
                                  ApplyPrimitiveProcedure
                                  ))

;; Gets the label from the closure stored in the 'proc register and returns it.
(define-struct: GetCompiledProcedureEntry ()
  #:transparent)

;; Constructs a closure, given the label, # of expected arguments,
;; and the set of lexical references into the environment that the
;; closure needs to close over.
(define-struct: MakeCompiledProcedure ([label : Symbol]
                                       [arity : Arity]
                                       [closed-vals : (Listof Natural)]
                                       [display-name : (U Symbol LamPositionalName)])
  #:transparent)


;; Constructs a closure shell.  Like MakeCompiledProcedure, but doesn't
;; bother with trying to capture the free variables.
(define-struct: MakeCompiledProcedureShell ([label : Symbol]
                                            [arity : Arity]
                                            [display-name : (U Symbol LamPositionalName)])
  #:transparent)





(define-struct: CallKernelPrimitiveProcedure ([operator : KernelPrimitiveName/Inline]

                                              [operands : (Listof (U OpArg ModuleVariable))]
                                              [expected-operand-types : (Listof OperandDomain)]
                                              ;; For each operand, #t will add code to typecheck the operand
                                              [typechecks? : (Listof Boolean)])
  #:transparent)


(define-struct: ApplyPrimitiveProcedure ([name : Symbol]) #:transparent)


(define-struct: MakeBoxedEnvironmentValue ([depth : Natural])
  #:transparent)


;; Capture the current environment, skipping skip frames.
(define-struct: CaptureEnvironment ([skip : Natural]
                                    [tag : (U DefaultContinuationPromptTag OpArg)]))

;; Capture the control stack, skipping skip frames.
(define-struct: CaptureControl ([skip : Natural]
                                [tag : (U DefaultContinuationPromptTag OpArg)]))




;; Primitive tests (used with TestAndBranch)
(define-type PrimitiveTest (U 
                            TestFalse
                            TestTrue
                            TestOne
                            TestZero
                            TestClosureArityMismatch
                            ))
(define-struct: TestFalse ([operand : OpArg]) #:transparent)
(define-struct: TestTrue ([operand : OpArg]) #:transparent)
(define-struct: TestOne ([operand : OpArg]) #:transparent)
(define-struct: TestZero ([operand : OpArg]) #:transparent)
(define-struct: TestClosureArityMismatch ([closure : OpArg]
                                          [n : OpArg]) #:transparent)



;; Check that the value in the prefix has been defined.
;; If not, raise an error and stop evaluation.
(define-struct: CheckToplevelBound! ([depth : Natural]
                                     [pos : Natural])
  #:transparent)

;; Check the closure procedure value in 'proc and make sure it's a closure
;; that can accept the right arguments (stored as a number in the argcount register.).
(define-struct: CheckClosureAndArity! ()
  #:transparent)

;; Check the primitive can accept the right arguments 
;; (stored as a number in the argcount register.).
(define-struct: CheckPrimitiveArity! () #:transparent)


;; Extends the environment with a prefix that holds
;; lookups to the namespace.
(define-struct: ExtendEnvironment/Prefix! ([names : (Listof (U False Symbol GlobalBucket ModuleVariable))])
  #:transparent)

;; Adjusts the environment by pushing the values in the
;; closure (held in the proc register) into itself.
(define-struct: InstallClosureValues! ([n : Natural])
  #:transparent)


(define-struct: SetFrameCallee! ([proc : OpArg])
  #:transparent)


;; Splices the list structure that lives in env[depth] into position.
;; Depth must evaluate to a natural.
(define-struct: SpliceListIntoStack! ([depth : OpArg])
  #:transparent)

;; Unsplices the length arguments on the stack, replacing with a list of that length.
;; Side effects: touches both the environment and argcount appropriately. 
(define-struct: UnspliceRestFromStack! ([depth : OpArg]
                                        [length : OpArg])
  #:transparent)


(define-struct: FixClosureShellMap! (;; depth: where the closure shell is located in the environment
                                     [depth : Natural] 
                                     
                                     [closed-vals : (Listof Natural)])
  #:transparent)

;; Raises an exception that says that we expected a number of values.
;; Assume that argcount is not equal to expected.
(define-struct: RaiseContextExpectedValuesError! ([expected : Natural])
  #:transparent)


;; Raises an exception that says that we're doing a
;; procedure application, but got sent an incorrect number.
(define-struct: RaiseArityMismatchError! ([proc : OpArg]
                                          [expected : Arity]
					  [received : OpArg])
  #:transparent)


;; Raises an exception that says that we're doing a
;; procedure application, but got sent an incorrect number.
(define-struct: RaiseOperatorApplicationError! ([operator : OpArg])
  #:transparent)


;; Raise a runtime error if we hit a use of an unimplemented kernel primitive.
(define-struct: RaiseUnimplementedPrimitiveError! ([name : Symbol])
  #:transparent)




;; Changes over the control located at the given argument from the structure in env[1]
(define-struct: RestoreControl! ([tag : (U DefaultContinuationPromptTag OpArg)]) #:transparent)

;; Changes over the environment located at the given argument from the structure in env[0]
(define-struct: RestoreEnvironment! () #:transparent)


;; Adds a continuation mark into the current top control frame.
(define-struct: InstallContinuationMarkEntry! () #:transparent)


;; Installs a module record into the machine
(define-struct: InstallModuleEntry! ([name : Symbol]
                                     [path : ModuleLocator]
                                     [entry-point : Symbol])
  #:transparent)


;; Mark that the module has been invoked.
(define-struct: MarkModuleInvoked! ([path : ModuleLocator])
  #:transparent)


;; Give an alternative locator to the module as a main module.
;; Assumes the module has already been installed.
(define-struct: AliasModuleAsMain! ([from : ModuleLocator])
  #:transparent)

;; Given the module locator, do any finalizing operations, like
;; setting up the module namespace.
(define-struct: FinalizeModuleInvokation! ([path : ModuleLocator])
  #:transparent)



(define-type PrimitiveCommand (U                                
                               CheckToplevelBound!
                               CheckClosureAndArity!
                               CheckPrimitiveArity!

                               ExtendEnvironment/Prefix!
                               InstallClosureValues!
                               FixClosureShellMap!
           
                               InstallContinuationMarkEntry!
                               
                               SetFrameCallee!
                               SpliceListIntoStack!
                               UnspliceRestFromStack!
                            
                               RaiseContextExpectedValuesError!
                               RaiseArityMismatchError!
			       RaiseOperatorApplicationError!
                               RaiseUnimplementedPrimitiveError!
                               
                               RestoreEnvironment!
                               RestoreControl!
                               
                               InstallModuleEntry!
                               MarkModuleInvoked!
                               AliasModuleAsMain!
                               FinalizeModuleInvokation!
                               ))




(define-type InstructionSequence (U Symbol
                                    LinkedLabel
                                    UnlabeledStatement
                                    instruction-sequence-list
                                    instruction-sequence-chunks))
(define-struct: instruction-sequence-list ([statements : (Listof Statement)])
  #:transparent)
(define-struct: instruction-sequence-chunks ([chunks : (Listof InstructionSequence)])
  #:transparent)
(define empty-instruction-sequence (make-instruction-sequence-list '()))


(define-predicate Statement? Statement)


(: statements (InstructionSequence -> (Listof Statement)))
(define (statements s)
  (reverse (statements-fold (inst cons Statement (Listof Statement))
                            '() s)))


(: statements-fold (All (A) ((Statement A -> A) A InstructionSequence -> A)))
(define (statements-fold f acc seq)
  (cond
   [(symbol? seq)
    (f seq acc)]
   [(LinkedLabel? seq)
    (f seq acc)]
   [(UnlabeledStatement? seq)
    (f seq acc)]
   [(instruction-sequence-list? seq)
    (foldl f acc (instruction-sequence-list-statements seq))]
   [(instruction-sequence-chunks? seq)
    (foldl (lambda: ([subseq : InstructionSequence] [acc : A])
             (statements-fold f acc subseq))
           acc
           (instruction-sequence-chunks-chunks seq))]))

            




(: append-instruction-sequences (InstructionSequence * -> InstructionSequence))
(define (append-instruction-sequences . seqs)
  (append-seq-list seqs))

(: append-2-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (append-2-sequences seq1 seq2)
  (make-instruction-sequence-chunks (list seq1 seq2)))

(: append-seq-list ((Listof InstructionSequence) -> InstructionSequence))
(define (append-seq-list seqs)
  (if (null? seqs)
      empty-instruction-sequence
      (make-instruction-sequence-chunks seqs)))








(define-predicate OpArg? OpArg)