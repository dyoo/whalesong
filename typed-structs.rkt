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
(define-type Statement (U Symbol  ;; label
                          AssignStatement
                          PerformStatement
                          TestStatement
                          BranchStatement
                          GotoStatement
                          SaveStatement
                          RestoreStatement))
(define-struct: AssignStatement () #:transparent)
(define-struct: PerformStatement () #:transparent)
(define-struct: TestStatement () #:transparent)
(define-struct: BranchStatement () #:transparent)
(define-struct: GotoStatement () #:transparent)
(define-struct: SaveStatement () #:transparent)
(define-struct: RestoreStatement () #:transparent)






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



(define-struct: basic-block ([name : Symbol] [stmts : (Listof Statement)]) #:transparent)
