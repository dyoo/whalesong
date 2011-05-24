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
                         Lam 
                         CaseLam
                         EmptyClosureReference
                         Seq 
                         Splice 
                         Begin0
                         App
                         Let1 
                         LetVoid 
                         LetRec
                         InstallValue
                         BoxEnv
                         WithContMark
                         ApplyValues
                         DefValues
                         PrimitiveKernelValue
                         Module
                         VariableReference
                         Require))


(define-struct: Module ([name : Symbol]
                        [path : ModuleLocator]
                        [prefix : Prefix]
                        [requires : (Listof ModuleLocator)]
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
                             [value : Expression]) #:transparent)

(define-struct: Branch ([predicate : Expression]
                        [consequent : Expression]
                        [alternative : Expression]) #:transparent)

(define-struct: CaseLam ([name : (U Symbol LamPositionalName)]
                         [clauses : (Listof (U Lam EmptyClosureReference))]
                         [entry-label : Symbol]) #:transparent)

(define-struct: Lam ([name : (U Symbol LamPositionalName)]
                     [num-parameters : Natural]
                     [rest? : Boolean]
                     [body : Expression]
                     [closure-map : (Listof Natural)]
                     [entry-label : Symbol]) #:transparent)

;; An EmptyClosureReference has enough information to create the lambda value,
;; assuming that the lambda's body has already been compiled.  The entry-label needs
;; to have been shared with an existing Lam, and the closure must be empty.
(define-struct: EmptyClosureReference ([name : (U Symbol LamPositionalName)]
                                       [num-parameters : Natural]
                                       [rest? : Boolean]
                                       [entry-label : Symbol]) #:transparent)



;; We may have more information about the lambda's name.  This will show it.
(define-struct: LamPositionalName ([name : Symbol] 
                                   [path : String]    ;; the source of the name
                                   [line : Natural] 
                                   [column : Natural] 
                                   [offset : Natural] 
                                   [span : Natural]) #:transparent)



(define-struct: Seq ([actions : (Listof Expression)]) #:transparent)
(define-struct: Splice ([actions : (Listof Expression)]) #:transparent)
(define-struct: Begin0 ([actions : (Listof Expression)]) #:transparent)
(define-struct: App ([operator : Expression]
                     [operands : (Listof Expression)]) #:transparent)

(define-struct: Let1 ([rhs : Expression]
                      [body : Expression])  #:transparent)

(define-struct: LetVoid ([count : Natural]
                         [body : Expression]
                         [boxes? : Boolean]) #:transparent)


;; During evaluation, the closures corresponding to procs are expected
;; to be laid out so that stack position 0 corresponds to procs[0],
;; stack position 1 to procs[1], and so on.
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



(define-struct: PrimitiveKernelValue ([id : Symbol]) #:transparent)


(define-struct: VariableReference ([toplevel : ToplevelRef]) #:transparent)


(define-struct: Require ([path : ModuleLocator]) #:transparent)



(: make-label (Symbol -> Symbol))
(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))