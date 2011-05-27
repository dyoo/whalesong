#lang typed/racket/base

(provide (all-defined-out))


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
                                           'null?
                                           'not
                                           'eq?))


(define-predicate KernelPrimitiveName/Inline? KernelPrimitiveName/Inline)



(: kernel-primitive-expected-operand-types (KernelPrimitiveName/Inline Natural -> (Listof OperandDomain)))
;; Given a primitive and the number of arguments, produces the list of expected domains.
;; TODO: do something more polymorphic.
(define (kernel-primitive-expected-operand-types prim arity)
  (cond
    [(eq? prim '+)
     (build-list arity (lambda (i) 'number))]

    [(eq? prim '-)
     (unless (> arity 0)
       (error '- "expects at least one argument, given ~a" arity))
     (build-list arity (lambda (i) 'number))]

    [(eq? prim '*)
     (build-list arity (lambda (i) 'number))]

    [(eq? prim '/)
     (unless (> arity 0)
       (error '/ "expects at least one argument, given ~a" arity))
     (build-list arity (lambda (i) 'number))]

    [(eq? prim 'add1)
     (unless (= arity 1)
       (error 'add1 "expects exactly one argument, given ~a" arity))
     (list 'number)]
    
    [(eq? prim 'sub1)
     (unless (= arity 1)
       (error 'sub1 "expects exactly one argument, given ~a" arity))
     (list 'number)]
    
    [(eq? prim '<)
     (build-list arity (lambda (i) 'number))]
    
    [(eq? prim '<=)
     (build-list arity (lambda (i) 'number))]
    
    [(eq? prim '=)
     (build-list arity (lambda (i) 'number))]
    
    [(eq? prim '>)
     (build-list arity (lambda (i) 'number))]
    
    [(eq? prim '>=)
     (build-list arity (lambda (i) 'number))]

    [(eq? prim 'cons)
     (unless (= arity 2)
       (error 'cons "expects exactly two arguments, given ~a" arity))
     (list 'any 'any)]

    [(eq? prim 'car)
     (unless (= arity 1)
       (error 'car "expects exactly one argument, given ~a" arity))
     (list 'pair)]
    
    [(eq? prim 'cdr)
     (unless (= arity 1)
       (error 'cdr "expects exactly one argument, given ~a" arity))
     (list 'pair)]
    
    [(eq? prim 'list)
     (build-list arity (lambda (i) 'any))]
    
    [(eq? prim 'null?)
     (unless (= arity 1)
       (error 'null? "expects exactly one argument, given ~a" arity))
     (list 'any)]

    [(eq? prim 'not)
     (unless (= arity 1)
       (error 'not "expects exactly one argument, given ~a" arity))
     (list 'any)]

    [(eq? prim 'eq?)
     (unless (= arity 2)
       (error 'eq? "expects exactly two arguments, given ~a" arity))
     (list 'any 'any)]))