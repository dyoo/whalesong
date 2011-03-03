#lang typed/racket/base
(provide (all-defined-out))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registers of the machine:

(define-type StackRegisterSymbol (U 'control 'env))
(define-type AtomicRegisterSymbol (U 'val 'proc))
(define-type RegisterSymbol (U StackRegisterSymbol AtomicRegisterSymbol))

(define-predicate AtomicRegisterSymbol? AtomicRegisterSymbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; An operation can refer to the following:
(define-type OpArg (U Const ;; an constant
                      Label ;; an label
                      Reg   ;; an register 
                      EnvLexicalReference
                      EnvWholePrefixReference))

(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : RegisterSymbol])
  #:transparent)
(define-struct: Const ([const : Any])
  #:transparent)
(define-struct: EnvLexicalReference ([depth : Natural])
  #:transparent)
(define-struct: EnvWholePrefixReference ([depth : Natural])
  #:transparent)

;; An environment reference
(define-type EnvReference (U EnvLexicalReference
                             EnvWholePrefixReference))






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
(define-struct: PushEnvironment ([n : Natural])
  #:transparent)


(define-struct: PopControlFrame () 
  #:transparent)

;; Adding a frame for getting back after procedure application.
(define-struct: PushControlFrame ([label : Symbol]) 
  #:transparent)

(define-struct: GotoStatement ([target : (U Label Reg)]) 
  #:transparent)

(define-struct: PerformStatement ([op : PrimitiveCommand]
                                  [rands : (Listof (U Label Reg Const))])
  #:transparent)
(define-struct: TestAndBranchStatement ([op : PrimitiveTest]
                                        [register-rand : RegisterSymbol]
                                        [label : Symbol])
  #:transparent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Operators

;; The operators that return values, that are used in AssignPrimopStatement.
(define-type PrimitiveOperator (U GetCompiledProcedureEntry
                                  MakeCompiledProcedure
                                  ApplyPrimitiveProcedure
                                  LookupLexicalAddress
                                  LookupToplevelAddress
                                  GetControlStackLabel))

;; Gets the label from the closure stored in the 'proc register and returns it.
(define-struct: GetCompiledProcedureEntry ()
  #:transparent)

;; Constructs a closure, given the label and the set of lexical references
;; into the environment that the closure needs to close over.
(define-struct: MakeCompiledProcedure ([label : Symbol]
                                       [closed-vals : (Listof EnvReference)])
  #:transparent)

;; Applies the primitive procedure that's stored in the proc register, using
;; the arity number of values that are bound in the environment as arguments
;; to that primitive.
(define-struct: ApplyPrimitiveProcedure ([arity : Natural])
  #:transparent)

;; Gets the value stored at the given depth in the environment.
(define-struct: LookupLexicalAddress ([depth : Natural])
  #:transparent)

;; Looks up the value in the prefix installed in the environment.    
(define-struct: LookupToplevelAddress ([depth : Natural]
                                       [pos : Natural]
                                       [name : Symbol])
  #:transparent)

;; Gets the return address embedded at the top of the control stack.
(define-struct: GetControlStackLabel ()
  #:transparent)



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


(define-type PrimitiveCommand (U                                
                               ;; depth pos symbol
                               ;; Assign the value in the val register into
                               ;; the prefix installed at (depth, pos).
                               'toplevel-set!
                                            
                               ;; depth pos symbol -> void
                               ;; Check that the value in the prefix has been defined.
                               ;; If not, raise an error and stop evaluation.
                               'check-bound!                              
                               
                               ;; (listof symbol) -> void
                               ;; Extends the environment with a prefix that holds
                               ;; lookups to the namespace.
                               'extend-environment/prefix!
                               
                               ;; register -> void
                               ;; Adjusts the environment by pushing the values in the
                               ;; closure (held in the register) into itself.
                               'install-closure-values!
                               ))




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



;; Targets
(define-type Target (U RegisterSymbol ControlTarget EnvOffset))
(define-struct: EnvOffset ([depth : Natural]) #:transparent)
(define-struct: ControlTarget () #:transparent)


;; Linkage
(define-type Linkage (U 'return 
                        'next
                        Symbol))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly

(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) 
  #:transparent)

