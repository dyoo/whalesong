#lang typed/racket/base
(provide (all-defined-out))

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
                                       [op : Symbol]
                                       [rands : (Listof (U Label Reg Const))])
  #:transparent)
(define-struct: PerformStatement ([op : Symbol]
                                  [rands : (Listof (U Label Reg Const))]) #:transparent)
(define-struct: TestStatement ([op : (U 'false? 'primitive-procedure?)]
                               [register-rand : Symbol]) #:transparent)
(define-struct: BranchLabelStatement ([label : Symbol]) #:transparent)
(define-struct: GotoStatement ([target : (U Label Reg)]) #:transparent)
(define-struct: SaveStatement ([reg : Symbol]) #:transparent)
(define-struct: RestoreStatement ([reg : Symbol]) #:transparent)

(define-struct: Label ([name : Symbol]))
(define-struct: Reg ([name : Symbol]))
(define-struct: Const ([const : Any]))

(define-type OpArg (U Const Label Reg))





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



(define-struct: BasicBlock ([name : Symbol] 
                            [stmts : (Listof UnlabeledStatement)]) #:transparent)
