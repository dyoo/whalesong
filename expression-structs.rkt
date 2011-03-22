#lang typed/racket/base
(require "lexical-structs.rkt")

(provide (all-defined-out))

;; Expressions

(define-type ExpressionCore (U Top Constant 
                               ToplevelRef LocalRef
                               ToplevelSet
                               Branch Lam Seq App
                               Let1 
                               LetVoid 
                               InstallValue
                               BoxEnv))

(define-struct: Top ([prefix : Prefix]
                     [code : ExpressionCore]) #:transparent)

(define-struct: Constant ([v : Any]) #:transparent)

(define-struct: ToplevelRef ([depth : Natural]
                             [pos : Natural])
  #:transparent)

(define-struct: LocalRef ([depth : Natural]
                          [unbox? : Boolean])
  #:transparent)

(define-struct: ToplevelSet ([depth : Natural]
                             [pos : Natural]
                             [name : Symbol] 
                             [value : ExpressionCore]) #:transparent)

(define-struct: Branch ([predicate : ExpressionCore]
                        [consequent : ExpressionCore]
                        [alternative : ExpressionCore]) #:transparent)

(define-struct: Lam ([name : (U Symbol False)]
                     [num-parameters : Natural]
                     [body : ExpressionCore]
                     [closure-map : (Listof Natural)]) #:transparent)

(define-struct: Seq ([actions : (Listof ExpressionCore)]) #:transparent)
(define-struct: App ([operator : ExpressionCore]
                     [operands : (Listof ExpressionCore)]) #:transparent)

(define-struct: Let1 ([rhs : ExpressionCore]
                      [body : ExpressionCore])
  #:transparent)
(define-struct: LetVoid ([count : Natural]
                         [body : ExpressionCore]
                         [boxes? : Boolean])
  #:transparent)

(define-struct: InstallValue ([depth : Natural]
                              [body : ExpressionCore]
                              [box? : Boolean])
  #:transparent)


(define-struct: BoxEnv ([depth : Natural]
                        [body : ExpressionCore])
  #:transparent)



(: last-exp? ((Listof Expression) -> Boolean))
(define (last-exp? seq) 
  (null? (cdr seq)))

(: first-exp ((Listof Expression) -> Expression))
(define (first-exp seq) (car seq))

(: rest-exps ((Listof Expression) -> (Listof Expression)))
(define (rest-exps seq) (cdr seq))


(define-type Expression (U ExpressionCore))
