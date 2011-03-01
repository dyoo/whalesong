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
                                 PushControl))
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


(define-struct: PopEnv ([n : Natural]) #:transparent)
(define-struct: PopControl () #:transparent)

(define-struct: PushEnv () #:transparent)
(define-struct: PushControl () #:transparent)


(define-struct: GotoStatement ([target : (U Label Reg)]) 
  #:transparent)


(define-struct: PerformStatement ([op : PerformOperator]
                                  [rands : (Listof (U Label Reg Const))]) #:transparent)
(define-struct: TestStatement ([op : TestOperator]
                               [register-rand : RegisterSymbol]) #:transparent)
(define-struct: BranchLabelStatement ([label : Symbol]) #:transparent)



(define-type PrimitiveOperator (U 'compiled-procedure-entry
                                  'compiled-procedure-env
                                  'make-compiled-procedure

                                  'false?
                                  'cons
                                  'list
                                  'apply-primitive-procedure
                                  
                                  'lexical-address-lookup
                                  'toplevel-lookup
                                  
                                  'read-control-label
                                  
                                  'extend-environment
                                  'extend-environment/prefix))

(define-type TestOperator (U 'false? 'primitive-procedure?))

(define-type PerformOperator (U 'toplevel-set!
                                'lexical-address-set!
                                'check-bound!))




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
(define-struct: ControlTarget ())
(define-struct: EnvOffset ([depth : Natural]
                           [pos : Natural]))

;; Linkage
(define-type Linkage (U 'return 
                        'next
                        Symbol))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembly

(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) #:transparent)

