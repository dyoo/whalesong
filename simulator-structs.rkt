#lang typed/racket/base

(provide (all-defined-out))

(require "il-structs.rkt"
         "lexical-structs.rkt")


(define-type PrimitiveValue (Rec PrimitiveValue (U String Number Symbol Boolean
                                                   Null Void
                                                   undefined
                                                   
                                                   primitive-proc 
                                                   closure
                                                   
                                                   (Vectorof PrimitiveValue)
                                                   MutablePair
                                                   
                                                   )))
(define-type SlotValue (U PrimitiveValue 
                          (Boxof PrimitiveValue)
                          toplevel
                          CapturedControl
                          CapturedEnvironment))


(define-struct: MutablePair ([h : PrimitiveValue]
                             [t : PrimitiveValue])
  #:mutable #:transparent)

;; For continuation capture:
(define-struct: CapturedControl ([frames : (Listof frame)]))
(define-struct: CapturedEnvironment ([vals : (Listof SlotValue)]))


(define-struct: machine ([val : SlotValue]
                         [proc : SlotValue]
                         [env : (Listof SlotValue)]
                         [control : (Listof frame)]

                         [pc : Natural]                  ;; program counter
                         [text : (Vectorof Statement)]   ;; text of the program

                         ;; other metrics for debugging
                         [stack-size : Natural]
                         
                         ;; compute position from label
                         [jump-table : (HashTable Symbol Natural)]
                         )
  #:transparent
  #:mutable)


(define-struct: frame ([return : Symbol]
                       ;; The procedure being called.  Used to optimize self-application
                       [proc : (U closure #f)]
                       ;; TODO: add continuation marks
                       )
  #:transparent)

(define-struct: toplevel ([names : (Listof (U #f Symbol ModuleVariable))]
                          [vals : (Listof PrimitiveValue)])
  #:transparent
  #:mutable)





;; Primitive procedure wrapper
(define-struct: primitive-proc ([f : (machine PrimitiveValue * -> PrimitiveValue)])
  #:transparent)



;; Compiled procedure closures
(define-struct: closure ([label : Symbol]
                         [arity : Natural]
                         [vals : (Listof SlotValue)]
                         [display-name : (U Symbol False)])
  #:transparent
  #:mutable)





;; undefined value
(define-struct: undefined ()
  #:transparent)



(define-predicate PrimitiveValue? PrimitiveValue)
