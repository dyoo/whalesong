#lang typed/racket/base
(require "lexical-structs.rkt")

(provide (all-defined-out))

;; Expressions

(define-type Expression (U Top Constant 
                               ToplevelRef LocalRef
                               ToplevelSet
                               Branch CaseLam Lam Seq Splice App
                               Let1 
                               LetVoid 
                               LetRec
                               InstallValue
                               BoxEnv
                               WithContMark
                               ApplyValues
                               DefValues))

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

(define-struct: CaseLam ([name : (U Symbol False)]
                         [clauses : (Listof Lam)]
                         [entry-label : Symbol]) #:transparent)

(define-struct: Lam ([name : (U Symbol False)]
                     [num-parameters : Natural]
                     [rest? : Boolean]
                     [body : Expression]
                     [closure-map : (Listof Natural)]
                     [entry-label : Symbol]) #:transparent)

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
