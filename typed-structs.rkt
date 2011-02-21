#lang typed/racket/base
(provide (all-defined-out))


;; Expressions

(define-type Expression (U Constant Quote Var Assign Branch Def Lam Seq App))
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
                     [body : (Listof Expression)]) #:transparent)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; instruction sequences
(define-type UnlabeledStatement (U 
                                 AssignImmediateStatement
                                 AssignPrimOpStatement
                                 PerformStatement
                                 TestStatement
                                 BranchLabelStatement
                                 GotoStatement
                                 SaveStatement
                                 RestoreStatement))
(define-type Statement (U UnlabeledStatement
                          Symbol  ;; label
                          ))
(define-struct: AssignImmediateStatement ([target : Symbol]
                                          [value : (U Const Reg Label)])
  #:transparent)
(define-struct: AssignPrimOpStatement ([target : Symbol]
                                       [op : PrimitiveOperator]
                                       [rands : (Listof (U Label Reg Const))])
  #:transparent)
(define-struct: PerformStatement ([op : PerformOperator]
                                  [rands : (Listof (U Label Reg Const))]) #:transparent)
(define-struct: TestStatement ([op : TestOperator]
                               [register-rand : Symbol]) #:transparent)
(define-struct: BranchLabelStatement ([label : Symbol]) #:transparent)
(define-struct: GotoStatement ([target : (U Label Reg)]) #:transparent)
(define-struct: SaveStatement ([reg : Symbol]) #:transparent)
(define-struct: RestoreStatement ([reg : Symbol]) #:transparent)

(define-struct: Label ([name : Symbol])
  #:transparent)
(define-struct: Reg ([name : Symbol])
  #:transparent)
(define-struct: Const ([const : Any])
  #:transparent)

(define-type OpArg (U Const Label Reg))


(define-type PrimitiveOperator (U 'compiled-procedure-entry
                                  'compiled-procedure-env
                                  'make-compiled-procedure

                                  'false?
                                  'cons
                                  'list
                                  'apply-primitive-procedure
                                  
                                  'lexical-address-lookup
                                  'toplevel-lookup
                                  
                                  'extend-environment
                                  'extend-environment/prefix))

(define-type TestOperator (U 'false? 'primitive-procedure?))

(define-type PerformOperator (U 'toplevel-set!
                                'lexical-address-set!
                                'check-bound!))




(define-type InstructionSequence (U Symbol instruction-sequence))
(define-struct: instruction-sequence ([needs : (Listof Symbol)]
                                      [modifies : (Listof Symbol)]
                                      [statements : (Listof Statement)]) #:transparent)
(define empty-instruction-sequence (make-instruction-sequence '() '() '()))

(: make-label (Symbol -> Symbol))
(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))


(: registers-needed (InstructionSequence -> (Listof Symbol)))
(define (registers-needed s)
  (if (symbol? s) '() (instruction-sequence-needs s)))

(: registers-modified (InstructionSequence -> (Listof Symbol)))
(define (registers-modified s)
  (if (symbol? s) '() (instruction-sequence-modifies s)))

(: statements (InstructionSequence -> (Listof Statement)))
(define (statements s)
  (if (symbol? s) (list s) (instruction-sequence-statements s)))



;; Targets
(define-type Target Symbol)

;; Linkage
(define-type Linkage (U 'return 'next Symbol))





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