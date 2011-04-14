#lang typed/racket/base
(provide (all-defined-out))

(require "lexical-structs.rkt"
         "kernel-primitives.rkt")




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
                      ))


;; Targets: these are the allowable lhs's for an assignment.
(define-type Target (U AtomicRegisterSymbol 
                       EnvLexicalReference
                       EnvPrefixReference
                       PrimitivesReference                   
                       ControlFrameTemporary))


;; When we need to store a value temporarily in the top control frame, we can use this as a target.
(define-struct: ControlFrameTemporary ([name : (U 'pendingContinuationMarkKey)])
  #:transparent)



(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : AtomicRegisterSymbol])
  #:transparent)
(define-struct: Const ([const : Any])
  #:transparent)

;; Limited arithmetic on OpArgs
(define-struct: SubtractArg ([lhs : OpArg]
                             [rhs : OpArg])
  #:transparent)



(define-struct: PrimitivesReference ([name : Symbol])
  #:transparent)




;; instruction sequences
(define-type UnlabeledStatement (U 
                                 
                                 AssignImmediateStatement
                                 AssignPrimOpStatement
                                 
                                 PerformStatement
                                 
                                 GotoStatement
                                 TestAndBranchStatement
                                 
                                 PopEnvironment
                                 PushEnvironment
                                 
                                 PushImmediateOntoEnvironment
                                 
                                 PushControlFrame/Generic
                                 PushControlFrame/Call
                                 PushControlFrame/Prompt

                                 PopControlFrame))


(define-type Statement (U UnlabeledStatement
                          Symbol      ;; label
                          LinkedLabel ;; Label with a reference to a multiple-return-value label
                          ))


(define-struct: LinkedLabel ([label : Symbol]
                             [linked-to : Symbol])
  #:transparent)


(define-struct: AssignImmediateStatement ([target : Target]
                                          [value : OpArg])
  #:transparent)
(define-struct: AssignPrimOpStatement ([target : Target]
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
(define-struct: PushControlFrame/Call ([label : (U Symbol LinkedLabel)]) 
  #:transparent)

(define-struct: PushControlFrame/Prompt ([tag : (U OpArg DefaultContinuationPromptTag)]
                                         [label : (U Symbol LinkedLabel)]
                                         ;; TODO: add handler and arguments
                                         )
  #:transparent)

(define-struct: DefaultContinuationPromptTag ()
  #:transparent)
(define default-continuation-prompt-tag 
  (make-DefaultContinuationPromptTag))




(define-struct: GotoStatement ([target : (U Label Reg)]) 
  #:transparent)

(define-struct: PerformStatement ([op : PrimitiveCommand])
  #:transparent)

(define-struct: TestAndBranchStatement ([op : PrimitiveTest]
                                        [register : AtomicRegisterSymbol]
                                        [label : Symbol])
  #:transparent)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Operators

;; The operators that return values, that are used in AssignPrimopStatement.
(define-type PrimitiveOperator (U GetCompiledProcedureEntry
                                  MakeCompiledProcedure
                                  MakeCompiledProcedureShell
                                  ApplyPrimitiveProcedure

                                  ;; Gets at the single-value-return address.
                                  GetControlStackLabel
                                  ;; Gets at the multiple-value-return address.
                                  GetControlStackLabel/MultipleValueReturn
                                  
                                  MakeBoxedEnvironmentValue

                                  CaptureEnvironment
                                  CaptureControl
                                  
                                  CallKernelPrimitiveProcedure))

;; Gets the label from the closure stored in the 'proc register and returns it.
(define-struct: GetCompiledProcedureEntry ()
  #:transparent)

;; Constructs a closure, given the label, # of expected arguments,
;; and the set of lexical references into the environment that the
;; closure needs to close over.
(define-struct: MakeCompiledProcedure ([label : Symbol]
                                       [arity : Arity]
                                       [closed-vals : (Listof Natural)]
                                       [display-name : (U Symbol False)])
  #:transparent)

;; Constructs a closure shell.  Like MakeCompiledProcedure, but doesn't
;; bother with trying to capture the free variables.
(define-struct: MakeCompiledProcedureShell ([label : Symbol]
                                            [arity : Arity]
                                            [display-name : (U Symbol False)])
  #:transparent)


;; Applies the primitive procedure that's stored in the proc register, using
;; the argcount number of values that are bound in the environment as arguments
;; to that primitive.
(define-struct: ApplyPrimitiveProcedure ()
  #:transparent)







(define-struct: CallKernelPrimitiveProcedure ([operator : KernelPrimitiveName]

                                              [operands : (Listof OpArg)]
                                              [expected-operand-types : (Listof OperandDomain)]
                                              ;; For each operand, #t will add code to typecheck the operand
                                              [typechecks? : (Listof Boolean)])
  #:transparent)



;; Gets the return address embedded at the top of the control stack.
(define-struct: GetControlStackLabel ()
  #:transparent)
(define-struct: GetControlStackLabel/MultipleValueReturn ()
  #:transparent)


(define-struct: MakeBoxedEnvironmentValue ([depth : Natural])
  #:transparent)


;; Capture the current environment, skipping skip frames.
(define-struct: CaptureEnvironment ([skip : Natural]
                                    [tag : (U DefaultContinuationPromptTag OpArg)]))

;; Capture the control stack, skipping skip frames.
(define-struct: CaptureControl ([skip : Natural]
                                [tag : (U DefaultContinuationPromptTag OpArg)]))




;; The following is used with TestStatement: each is passed the register-rand and
;; is expected to
(define-type PrimitiveTest (U 
                            ;; register -> boolean
                            ;; Meant to branch when the register value is false.
                            'false?

                            'one?
                            
                            ;; register -> boolean
                            ;; Meant to branch when the register value is a primitive
                            ;; procedure
                            'primitive-procedure?
                            ))



;; Check that the value in the prefix has been defined.
;; If not, raise an error and stop evaluation.
(define-struct: CheckToplevelBound! ([depth : Natural]
                                     [pos : Natural])
  #:transparent)

;; Check the closure procedure value in 'proc and make sure it can accept the
;; # of arguments (stored as a number in the val register.).
(define-struct: CheckClosureArity! ([arity : OpArg])
  #:transparent)
(define-struct: CheckPrimitiveArity! ([arity : OpArg])
  #:transparent)



;; Extends the environment with a prefix that holds
;; lookups to the namespace.
(define-struct: ExtendEnvironment/Prefix! ([names : (Listof (U Symbol ModuleVariable False))])
  #:transparent)

;; Adjusts the environment by pushing the values in the
;; closure (held in the proc register) into itself.
(define-struct: InstallClosureValues! ()
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

;; Changes over the control located at the given argument from the structure in env[1]
(define-struct: RestoreControl! ([tag : (U DefaultContinuationPromptTag OpArg)]) #:transparent)

;; Changes over the environment located at the given argument from the structure in env[0]
(define-struct: RestoreEnvironment! () #:transparent)


;; Adds a continuation mark into the current top control frame.
(define-struct: InstallContinuationMarkEntry! () #:transparent)

(define-type PrimitiveCommand (U                                
                               CheckToplevelBound!
                               CheckClosureArity!
                               CheckPrimitiveArity!

                               ExtendEnvironment/Prefix!
                               InstallClosureValues!
                               FixClosureShellMap!
           
                               InstallContinuationMarkEntry!
                               
                               SetFrameCallee!
                               SpliceListIntoStack!
                               UnspliceRestFromStack!
                               
                               RestoreEnvironment!
                               RestoreControl!))




(define-type InstructionSequence (U Symbol LinkedLabel instruction-sequence))
(define-struct: instruction-sequence ([statements : (Listof Statement)])
  #:transparent)
(define empty-instruction-sequence (make-instruction-sequence '()))

(: make-label (Symbol -> Symbol))
(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))


(: statements (InstructionSequence -> (Listof Statement)))
(define (statements s)
  (cond [(symbol? s) 
         (list s)]
        [(LinkedLabel? s)
         (list s)]
        [else
         (instruction-sequence-statements s)]))






;; Linkage
(define-struct: NextLinkage ())
(define next-linkage (make-NextLinkage))

(define-struct: ReturnLinkage ())
(define return-linkage (make-ReturnLinkage))

(define-struct: PromptLinkage ())
(define prompt-linkage (make-PromptLinkage))

(define-struct: LabelLinkage ([label : Symbol]))

(define-type Linkage (U NextLinkage
                        ReturnLinkage
                        PromptLinkage
                        LabelLinkage))












;; Static knowledge about a value

;; We try to keep at compile time a mapping from environment positions to
;; statically known things, to generate better code.
(define-struct: StaticallyKnownLam ([name : (U Symbol False)]
                                    [entry-point : Symbol]
                                    [arity : Arity]) #:transparent)

(define-type CompileTimeEnvironmentEntry 
  (U '?          ;; no knowledge
     Prefix      ;; placeholder: necessary since the toplevel lives in the environment too
     StaticallyKnownLam ;; The value is a known lam
     ModuleVariable     ;; The value is a known module variable
     Const
     ))

(define-type CompileTimeEnvironment (Listof CompileTimeEnvironmentEntry))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly

(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) 
  #:transparent)



(define-type Arity (U Natural ArityAtLeast (Listof (U Natural ArityAtLeast))))
(define-struct: ArityAtLeast ([value : Natural])
  #:transparent)

(define-predicate listof-atomic-arity? (Listof (U Natural ArityAtLeast)))




(define-predicate OpArg? OpArg)