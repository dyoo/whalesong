#lang typed/racket/base
(provide (all-defined-out))

(require "lexical-structs.rkt"
         "kernel-primitives.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registers of the machine:

(define-type StackRegisterSymbol (U 'control 'env))
(define-type AtomicRegisterSymbol (U 'val 'proc))
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
                      ))


;; Targets: these are the allowable lhs's for an assignment.
(define-type Target (U AtomicRegisterSymbol 
                       EnvLexicalReference
                       EnvPrefixReference
                       PrimitivesReference))



(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : AtomicRegisterSymbol])
  #:transparent)
(define-struct: Const ([const : Any])
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
                                 PushControlFrame
                                 PopControlFrame))

(define-type Statement (U UnlabeledStatement
                          Symbol  ;; label
                          ))

(define-struct: AssignImmediateStatement ([target : Target]
                                          [value : OpArg])
  #:transparent)
(define-struct: AssignPrimOpStatement ([target : Target]
                                       [op : PrimitiveOperator])
  #:transparent)


;; Pop n slots from the environment, skipping past a few first.
(define-struct: PopEnvironment ([n : Natural]
                                [skip : Natural])
  #:transparent)
(define-struct: PushEnvironment ([n : Natural]
                                 [unbox? : Boolean])
  #:transparent)


(define-struct: PopControlFrame () 
  #:transparent)

;; Adding a frame for getting back after procedure application.
;; The 'proc register must hold either #f or a closure at the time of
;; this call, as the control frame will hold onto the called procedure record.
(define-struct: PushControlFrame ([label : Symbol]) 
  #:transparent)

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

                                  GetControlStackLabel
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
                                       [arity : Natural]
                                       [closed-vals : (Listof Natural)]
                                       [display-name : (U Symbol False)])
  #:transparent)

;; Constructs a closure shell.  Like MakeCompiledProcedure, but doesn't
;; bother with trying to capture the free variables.
(define-struct: MakeCompiledProcedureShell ([label : Symbol]
                                            [arity : Natural]
                                            [display-name : (U Symbol False)])
  #:transparent)


;; Applies the primitive procedure that's stored in the proc register, using
;; the arity number of values that are bound in the environment as arguments
;; to that primitive.
(define-struct: ApplyPrimitiveProcedure ([arity : Natural])
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

(define-struct: MakeBoxedEnvironmentValue ([depth : Natural])
  #:transparent)


;; Capture the current environment, skipping skip frames.
(define-struct: CaptureEnvironment ([skip : Natural]))

;; Capture the control stack, skipping skip frames.
(define-struct: CaptureControl ([skip : Natural]))




;; The following is used with TestStatement: each is passed the register-rand and
;; is expected to
(define-type PrimitiveTest (U 
                            ;; register -> boolean
                            ;; Meant to branch when the register value is false.
                            'false?
                            
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

;; Check the closure procedure value in 'proc and make sure it can accept n values.
(define-struct: CheckClosureArity! ([arity : Natural])
  #:transparent)

;; Extends the environment with a prefix that holds
;; lookups to the namespace.
(define-struct: ExtendEnvironment/Prefix! ([names : (Listof (U Symbol ModuleVariable False))])
  #:transparent)

;; Adjusts the environment by pushing the values in the
;; closure (held in the proc register) into itself.
(define-struct: InstallClosureValues! ()
  #:transparent)

(define-struct: FixClosureShellMap! (;; depth: where the closure shell is located in the environment
                                     [depth : Natural] 
                                     
                                     [closed-vals : (Listof Natural)])
  #:transparent)

;; Changes over the control located at the given argument from the structure in env[1]
(define-struct: RestoreControl! ())

;; Changes over the environment located at the given argument from the structure in env[0]
(define-struct: RestoreEnvironment! ())



(define-type PrimitiveCommand (U                                
                               CheckToplevelBound!
                               CheckClosureArity!
                               ExtendEnvironment/Prefix!
                               InstallClosureValues!
                               FixClosureShellMap!
                               
                               RestoreEnvironment!
                               RestoreControl!))




(define-type InstructionSequence (U Symbol instruction-sequence))
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
  (if (symbol? s) (list s) (instruction-sequence-statements s)))






;; Linkage
(define-struct: NextLinkage ())
(define next-linkage (make-NextLinkage))

(define-struct: ReturnLinkage ())
(define return-linkage (make-ReturnLinkage))

(define-struct: LabelLinkage ([label : Symbol]))

(define-type Linkage (U NextLinkage
                        ReturnLinkage
                        LabelLinkage))












;; Static knowledge about a value

;; We try to keep at compile time a mapping from environment positions to
;; statically known things, to generate better code.
(define-struct: StaticallyKnownLam ([name : (U Symbol False)]
                                    [entry-point : Symbol]
                                    [arity : Natural]) #:transparent)

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

