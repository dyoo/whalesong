#lang typed/racket/base

(provide (all-defined-out))

(require "il-structs.rkt")


(define-type PrimitiveValue (Rec PrimitiveValue (U String Number Symbol Boolean
                                                   Null Void
                                                   primitive-proc 
                                                   closure
                                                   undefined

                                                   (Pairof PrimitiveValue PrimitiveValue)
                                                   )))
(define-type SlotValue (U PrimitiveValue toplevel))



(define-struct: machine ([val : SlotValue]
                         [proc : SlotValue]
                         [env : (Listof SlotValue)]
                         [control : (Listof frame)]

                         [pc : Natural]                  ;; program counter
                         [text : (Vectorof Statement)]   ;; text of the program

                         ;; other metrics for debugging
                         [stack-size : Natural]
                         )
  #:transparent)


(define-struct: frame ([return : Symbol]
                       ;; TODO: add continuation marks
                       )
  #:transparent)

(define-struct: toplevel ([vals : (Listof PrimitiveValue)])
  #:transparent
  #:mutable)





;; Primitive procedure wrapper
(define-struct: primitive-proc ([f : (PrimitiveValue * -> PrimitiveValue)])
  #:transparent)

;; Compiled procedure closures
(define-struct: closure ([label : Symbol]
                         [arity : Natural]
                         [vals : (Listof SlotValue)])
  #:transparent)

;; undefined value
(define-struct: undefined ()
  #:transparent)



(define-predicate PrimitiveValue? PrimitiveValue)
