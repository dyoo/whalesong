#lang typed/racket/base

(provide (all-defined-out))

(require "arity-structs.rkt"
         "../type-helpers.rkt")
(define-type OperandDomain (U 'number
                              'string
                              'box
                              'list
                              'pair
                              'any))


;; The following are primitives that the compiler knows about:
(define-type KernelPrimitiveName (U '+
                                    '-
                                    '*
                                    '/
                                    'add1
                                    'sub1
				    'abs
                                    '<
                                    '<=
                                    '=
                                    '>
                                    '>=
                                    'cons
                                    'car
                                    'cdr
				    'cadr
				    'caddr
                                    'list
                                    'list?
                                    'list*
                                    'list->vector
                                    'vector->list
                                    'vector
                                    'vector-length
                                    'vector-ref
                                    'vector-set!
                                    'make-vector
                                    'equal?
                                    'member
                                    'append
                                    'reverse
                                    'length
				    'pair?
                                    'null?
                                    'not
                                    'eq?
				    'remainder
				    'display
				    'newline
				    'call/cc
				    'box
				    'unbox
				    'set-box!
				    'string-append
				    'current-continuation-marks
				    'continuation-mark-set->list
				    'values
				    'call-with-values
                                    'apply
                                    

                                    'for-each
                                    'current-print

                                    'make-struct-type
                                    'current-inspector
                                    'make-struct-field-accessor
                                    'make-struct-field-mutator
                                    
                                    'gensym
                                    'srcloc
                                    'make-srcloc
                                    'srcloc-source
                                    'srcloc-line
                                    'srcloc-column
                                    'srcloc-position
                                    'srcloc-span
                                    ))
(define-predicate KernelPrimitiveName? KernelPrimitiveName)


;; These are the primitives that we know how to inline.
(define-type KernelPrimitiveName/Inline (U '+
                                           '-
                                           '*
                                           '/
                                           'add1
                                           'sub1
                                           '<
                                           '<=
                                           '=
                                           '>
                                           '>=
                                           'cons
                                           'car
                                           'cdr
                                           'list
                                           'list?
                                           'pair?
                                           'null?
                                           'not
                                           'eq?))

(ensure-type-subsetof KernelPrimitiveName/Inline KernelPrimitiveName)


(define-predicate KernelPrimitiveName/Inline? KernelPrimitiveName/Inline)

(define-struct: IncorrectArity ([expected : Arity]))


(: kernel-primitive-expected-operand-types (KernelPrimitiveName/Inline Natural -> (U (Listof OperandDomain)
                                                                                     IncorrectArity)))
;; Given a primitive and the number of arguments, produces the list of expected domains.
;; TODO: do something more polymorphic.
(define (kernel-primitive-expected-operand-types prim arity)
  (cond
    [(eq? prim '+)
     (build-list arity (lambda (i) 'number))]

    [(eq? prim '-)
     (cond [(> arity 0)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 1))])]

    [(eq? prim '*)
     (build-list arity (lambda (i) 'number))]

    [(eq? prim '/)
     (cond [(> arity 0)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 1))])]

    [(eq? prim 'add1)
     (cond [(= arity 1)
            (list 'number)]
           [else
            (make-IncorrectArity (make-ArityAtLeast 1))])]
    
    [(eq? prim 'sub1)
     (cond [(= arity 1)
            (list 'number)]
           [else
            (make-IncorrectArity (make-ArityAtLeast 1))])]
                           
    [(eq? prim '<)
     (cond [(>= arity 2)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 2))])]
    
    [(eq? prim '<=)
     (cond [(>= arity 2)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 2))])]
    
    [(eq? prim '=)
     (cond [(>= arity 2)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 2))])]
    
    [(eq? prim '>)
     (cond [(>= arity 2)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 2))])]
    
    [(eq? prim '>=)
     (cond [(>= arity 2)
            (build-list arity (lambda (i) 'number))]
           [else
            (make-IncorrectArity (make-ArityAtLeast 2))])]

    [(eq? prim 'cons)
     (list 'any 'any)]

    [(eq? prim 'car)
     (list 'pair)]
    
    [(eq? prim 'cdr)
     (list 'pair)]
    
    [(eq? prim 'list)
     (build-list arity (lambda (i) 'any))]

    [(eq? prim 'list?)
     (list 'any)]

    [(eq? prim 'pair?)
     (list 'any)]

    [(eq? prim 'null?)
     (list 'any)]

    [(eq? prim 'not)
     (list 'any)]

    [(eq? prim 'eq?)
     (list 'any 'any)]))