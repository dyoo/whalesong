#lang typed/racket/base

(provide (all-defined-out))

(require "il-structs.rkt"
         "lexical-structs.rkt")


(define-type PrimitiveValue (Rec PrimitiveValue (U String Number Symbol Boolean
                                                   Null VoidValue
                                                   undefined
                                                   
                                                   primitive-proc 
                                                   closure
                                                   
                                                   (Vectorof PrimitiveValue)
                                                   MutablePair

                                                   ContinuationMarkSet
                                                   )))
(define-type SlotValue (U PrimitiveValue 
                          (Boxof PrimitiveValue)
                          toplevel
                          CapturedControl
                          CapturedEnvironment))


(define-struct: VoidValue () #:transparent)
(define the-void-value (make-VoidValue))


(define-struct: MutablePair ([h : PrimitiveValue]
                             [t : PrimitiveValue])
  #:mutable #:transparent)

;; For continuation capture:
(define-struct: CapturedControl ([frames : (Listof frame)]))
(define-struct: CapturedEnvironment ([vals : (Listof SlotValue)]))


(define-struct: machine ([val : SlotValue]
                         [proc : SlotValue]
                         [argcount : SlotValue]
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



(define-type frame (U GenericFrame CallFrame PromptFrame))


(define-struct: GenericFrame ([temps : (HashTable Symbol PrimitiveValue)]
                              [marks : (HashTable PrimitiveValue PrimitiveValue)])
  #:transparent)


(define-struct: CallFrame ([return : (U Symbol LinkedLabel)]
                           ;; The procedure being called.  Used to optimize self-application
                           [proc : (U closure #f)]
                           ;; TODO: add continuation marks
                           [temps : (HashTable Symbol PrimitiveValue)]
                           [marks : (HashTable PrimitiveValue PrimitiveValue)])
  #:transparent
  #:mutable)  ;; mutable because we want to allow mutation of proc.

(define-struct: PromptFrame ([tag : ContinuationPromptTagValue]
                             [return : (U Symbol LinkedLabel)]
                             [env-depth : Natural]
                             [temps : (HashTable Symbol PrimitiveValue)]
                             [marks : (HashTable PrimitiveValue PrimitiveValue)])
  #:transparent)


(: frame-temps (frame -> (HashTable Symbol PrimitiveValue)))
(define (frame-temps a-frame)
  (cond
    [(GenericFrame? a-frame)
     (GenericFrame-temps a-frame)]
    [(CallFrame? a-frame)
     (CallFrame-temps a-frame)]
    [(PromptFrame? a-frame)
     (PromptFrame-temps a-frame)]))


(: frame-marks (frame -> (HashTable PrimitiveValue PrimitiveValue)))
(define (frame-marks a-frame)
  (cond
    [(GenericFrame? a-frame)
     (GenericFrame-marks a-frame)]
    [(CallFrame? a-frame)
     (CallFrame-marks a-frame)]
    [(PromptFrame? a-frame)
     (PromptFrame-marks a-frame)]))


(: frame-tag (frame -> (U ContinuationPromptTagValue #f)))
(define (frame-tag a-frame)
  (cond
    [(GenericFrame? a-frame)
     #f]
    [(CallFrame? a-frame)
     #f]
    [(PromptFrame? a-frame)
     (PromptFrame-tag a-frame)]))



(define-struct: ContinuationPromptTagValue ([name : Symbol])
  #:transparent)

(define default-continuation-prompt-tag-value 
  (make-ContinuationPromptTagValue 'default-continuation-prompt))



(define-struct: ContinuationMarkSet ([marks : (Listof (Pairof PrimitiveValue PrimitiveValue))])
  #:transparent)







(define-struct: toplevel ([names : (Listof (U #f Symbol ModuleVariable))]
                          [vals : (Listof PrimitiveValue)])
  #:transparent
  #:mutable)





;; Primitive procedure wrapper
(define-struct: primitive-proc ([f : (machine PrimitiveValue * -> PrimitiveValue)]
                                [arity : Arity]
                                [display-name : (U Symbol False)])
  #:transparent)




;; Compiled procedure closures
(define-struct: closure ([label : Symbol]
                         [arity : Arity]
                         [vals : (Listof SlotValue)]
                         [display-name : (U Symbol False)])
  #:transparent
  #:mutable)





;; undefined value
(define-struct: undefined ()
  #:transparent)



(define-predicate PrimitiveValue? PrimitiveValue)
(define-predicate frame? frame)