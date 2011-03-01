#lang typed/racket/base
(provide (all-defined-out))


;; Expressions

(define-type ExpressionCore (U Constant Quote Var Branch Def #;Lam Seq #;App))
(define-type Expression (U ExpressionCore #;Assign))
(define-struct: Constant ([v : Any]) #:transparent)
(define-struct: Quote ([text : Any]) #:transparent)
(define-struct: Var ([id : Symbol]) #:transparent)
(define-struct: Assign ([variable : Symbol]
                        [value : Expression]) #:transparent)
(define-struct: Branch ([predicate : Expression]
                        [consequent : Expression]
                        [alternative : Expression]) #:transparent)
(define-struct: Def ([variable : Symbol] 
                     [value : Expression]) #:transparent)
(define-struct: Lam ([parameters : (Listof Symbol)]
                     [body : Expression]) #:transparent)
(define-struct: Seq ([actions : (Listof Expression)]) #:transparent)
(define-struct: App ([operator : Expression]
                     [operands : (Listof Expression)]) #:transparent)

(: last-exp? ((Listof Expression) -> Boolean))
(define (last-exp? seq) 
  (null? (cdr seq)))

(: first-exp ((Listof Expression) -> Expression))
(define (first-exp seq) (car seq))

(: rest-exps ((Listof Expression) -> (Listof Expression)))
(define (rest-exps seq) (cdr seq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type StackRegisterSymbol (U 'control 'env))
(define-type RegisterSymbol (U StackRegisterSymbol 'val 'proc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : RegisterSymbol])
  #:transparent)
(define-struct: Const ([const : Any])
  #:transparent)

(define-struct: TopControlProcedure ()) 

(define-type OpArg (U Const Label Reg TopControlProcedure))

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


;;;;;;;;;;;;;;

;;  Lexical environments

;; A toplevel prefix contains a list of toplevel variables.
(define-struct: Prefix ([names : (Listof Symbol)])
  #:transparent)

;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.
(define-type CompileTimeEnvironment (Listof (U (Listof Symbol)
                                               Prefix)))
(define-type LexicalAddress (U LocalAddress PrefixAddress))

(define-struct: LocalAddress ([depth : Natural]
                              [pos : Natural])
  #:transparent)
(define-struct: PrefixAddress ([depth : Natural]
                               [pos : Natural]
                               [name : Symbol])
  #:transparent)