#lang typed/racket/base
(require "lexical-structs.rkt")

(provide (all-defined-out))


;; Expressions
(define-type Expression (U
                         Top
                         Constant 
                         ToplevelRef
                         LocalRef
                         ToplevelSet
                         Branch 
                         CaseLam
                         Lam 
                         Seq 
                         Splice 
                         App
                         Let1 
                         LetVoid 
                         LetRec
                         InstallValue
                         BoxEnv
                         WithContMark
                         ApplyValues
                         DefValues))

;; A ModuleName is an identifier for a Module.
(define-struct: ModuleName ([name : Symbol])
  #:transparent)

(define-struct: Provided ([name : Symbol]
                          [src-name : Symbol])
  #:transparent)

(define-struct: Module ([name : ModuleName]
                        [prefix : Prefix]
                        [requires : (Listof ModuleName)]
                        [provides : (Listof Provided)]
                        [code : Expression])
  #:transparent)


(define-struct: Top ([prefix : Prefix]
                     [code : Expression]) #:transparent)

(define-struct: Constant ([v : Any]) #:transparent)

(define-struct: ToplevelRef ([depth : Natural]
                             [pos : Natural]) #:transparent)

(define-struct: LocalRef ([depth : Natural]
                          [unbox? : Boolean]) #:transparent)

(define-struct: ToplevelSet ([depth : Natural]
                             [pos : Natural]
                             [name : Symbol] 
                             [value : Expression]) #:transparent)

(define-struct: Branch ([predicate : Expression]
                        [consequent : Expression]
                        [alternative : Expression]) #:transparent)

(define-struct: CaseLam ([name : (U Symbol LamPositionalName)]
                         [clauses : (Listof Lam)]
                         [entry-label : Symbol]) #:transparent)

(define-struct: Lam ([name : (U Symbol LamPositionalName)]
                     [num-parameters : Natural]
                     [rest? : Boolean]
                     [body : Expression]
                     [closure-map : (Listof Natural)]
                     [entry-label : Symbol]) #:transparent)


;; We may have more information about the lambda's name.  This will show it.
(define-struct: LamPositionalName ([name : Symbol] 
                                   [path : String] 
                                   [line : Natural] 
                                   [column : Natural] 
                                   [offset : Natural] 
                                   [span : Natural]) #:transparent)



(define-struct: Seq ([actions : (Listof Expression)]) #:transparent)
(define-struct: Splice ([actions : (Listof Expression)]) #:transparent)
(define-struct: App ([operator : Expression]
                     [operands : (Listof Expression)]) #:transparent)

(define-struct: Let1 ([rhs : Expression]
                      [body : Expression])  #:transparent)

(define-struct: LetVoid ([count : Natural]
                         [body : Expression]
                         [boxes? : Boolean]) #:transparent)

(define-struct: LetRec ([procs : (Listof Lam)]
                        [body : Expression]) #:transparent)

(define-struct: InstallValue ([count : Natural] ;; how many values to install
                              [depth : Natural] ;; how many slots to skip
                              [body : Expression]
                              [box? : Boolean])  #:transparent)


(define-struct: BoxEnv ([depth : Natural]
                        [body : Expression]) #:transparent)



(define-struct: WithContMark ([key : Expression]
                              [value : Expression]
                              [body : Expression]) #:transparent)


(define-struct: ApplyValues ([proc : Expression]
                             [args-expr : Expression]) #:transparent)


;; Multiple value definition
(define-struct: DefValues ([ids : (Listof ToplevelRef)]
                           [rhs : Expression]) #:transparent)



(: last-exp? ((Listof Expression) -> Boolean))
(define (last-exp? seq) 
  (null? (cdr seq)))

(: first-exp ((Listof Expression) -> Expression))
(define (first-exp seq) (car seq))

(: rest-exps ((Listof Expression) -> (Listof Expression)))
(define (rest-exps seq) (cdr seq))




(: make-label (Symbol -> Symbol))
(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))