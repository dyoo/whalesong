#lang typed/racket/base
(provide (all-defined-out))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registers of the machine:

(define-type StackRegisterSymbol (U 'control 'env))
(define-type AtomicRegisterSymbol (U 'val 'proc))
(define-type RegisterSymbol (U StackRegisterSymbol AtomicRegisterSymbol))


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







;; instruction sequences
(define-type UnlabeledStatement (U 
                                 AssignImmediateStatement
                                 AssignPrimOpStatement
                                 GotoStatement
                                 PerformStatement
                                 TestStatement
                                 BranchLabelStatement
                                 PopEnv
                                 PopControl
                                 PushEnv
                                 PushControlFrame))
(define-type Statement (U UnlabeledStatement
                          Symbol  ;; label
                          ))

(define-struct: AssignImmediateStatement ([target : Target]
                                          [value : OpArg])
  #:transparent)
(define-struct: AssignPrimOpStatement ([target : Target]
                                       [op : PrimitiveOperator]
                                       [rands : (Listof OpArg)])
  #:transparent)


(define-struct: PopEnv ([n : Natural]
                        [skip : Natural])
  #:transparent)
(define-struct: PopControl () 
  #:transparent)
(define-struct: PushEnv ([n : Natural])
  #:transparent)

;; Adding a frame for getting back after procedure application.
(define-struct: PushControlFrame ([label : Symbol]) 
  #:transparent)


(define-struct: GotoStatement ([target : (U Label Reg)]) 
  #:transparent)


(define-struct: PerformStatement ([op : PrimitiveCommand]
                                  [rands : (Listof (U Label Reg Const))]) #:transparent)
(define-struct: TestStatement ([op : PrimitiveTest]
                               [register-rand : RegisterSymbol]) #:transparent)
(define-struct: BranchLabelStatement ([label : Symbol]) #:transparent)



(define-type PrimitiveOperator (U 
                                
                                ;; register -> label
                                ;; Get the label from the closure stored in
                                ;; the register and return it.
                                'compiled-procedure-entry
                                
                                ;; label LexicalReference * -> closure
                                'make-compiled-procedure                                
                                  
                                ;; primitive-procedure arity -> any
                                'apply-primitive-procedure
                                
                                ;; depth -> any
                                ;; Lookup the value in the environment
                                'lexical-address-lookup
                                
                                ;; depth pos symbol -> any
                                ;; lookup the value in the prefix installed in the
                                ;; environment.
                                'toplevel-lookup
                                
                                ;; -> label
                                ;; Grabs the label embedded in the top
                                ;; of the control stack
                                'read-control-label 
                                ))

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

