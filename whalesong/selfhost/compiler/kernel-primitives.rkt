#lang whalesong (require "../selfhost-lang.rkt")

(provide (all-defined-out))

(require "arity-structs.rkt"
         "lexical-structs.rkt"
         ; "../type-helpers.rkt"
         )




(: kernel-module-name? (ModuleLocator -> Boolean))
;; Produces true if the module is hardcoded.
(define (kernel-module-name? name)

  
  (: kernel-locator? (ModuleLocator -> Boolean))
  (define (kernel-locator? locator)
    (or (and (eq? (ModuleLocator-name locator) '#%kernel)
             (eq? (ModuleLocator-real-path locator) '#%kernel))
        (eq? (ModuleLocator-name locator)
             'whalesong/lang/kernel.rkt)
        
        ;; HACK HACK HACK
        ;; This is for srcloc:
        (eq? (ModuleLocator-name locator)
             'collects/racket/private/kernstruct.rkt)))


  (: paramz-locator? (ModuleLocator -> Boolean))
  (define (paramz-locator? locator)
    (or (and (eq? (ModuleLocator-name locator) '#%paramz)
             (eq? (ModuleLocator-real-path locator) '#%paramz))))


  (: kernel-module-locator? (ModuleLocator -> Boolean))
  ;; Produces true if the given module locator should be treated as a primitive root one
  ;; that is implemented by us.
  (define (kernel-module-locator? locator)
    (or (kernel-locator? locator)
        (paramz-locator? locator)))

  
  (kernel-module-locator? name))



;; Given a kernel-labeled ModuleVariable, returns the kernel name for it.
(: kernel-module-variable->primitive-name (ModuleVariable -> Symbol))
(define (kernel-module-variable->primitive-name a-modvar)
  ;; FIXME: remap if the module is something else like whalesong/unsafe/ops

  (ModuleVariable-name a-modvar))










(define-type OperandDomain (U 'number
                              'string
                              'vector
                              'box
                              'list
                              'pair
                              'caarpair
                              'any))


;; The following are primitives that the compiler knows about:
(define KernelPrimitiveNames (list '+
                                    '-
                                    '*
                                    '/
                                    'zero?
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

                                    
                                    'caar
                                    'cdar
                                    'cadr
                                    'cddr
                                    'caaar
                                    'cdaar
                                    'cadar
                                    'cddar
                                    'caadr
                                    'cdadr
                                    'caddr
                                    'cdddr
                                    'caaaar
                                    'cdaaar
                                    'cadaar
                                    'cddaar
                                    'caadar
                                    'cdadar
                                    'caddar
                                    'cdddar
                                    'caaadr
                                    'cdaadr
                                    'cadadr
                                    'cddadr
                                    'caaddr
                                    'cdaddr
                                    'cadddr
                                    'cddddr

                                    
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
                                    'memq
                                    'memv
                                    'memf
                                    'append
                                    'reverse
                                    'length
				    'pair?
                                    'null?
                                    'not
                                    'eq?
                                    'eqv?
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

				    'error
                                    'raise-type-error
                                    'raise-mismatch-error
                                    'struct:exn:fail
                                    'prop:exn:srclocs
                                    'make-exn
                                    'make-exn:fail
                                    'make-exn:fail:contract
                                    'make-exn:fail:contract:arity
                                    'make-exn:fail:contract:variable
                                    'make-exn:fail:contract:divide-by-zero

                                    'exn:fail?
                                    'exn:fail:contract?
                                    'exn:fail:contract:arity?
                                    
                                    'exn-message
                                    'exn-continuation-marks

                                    'hash?
                                    'hash-equal?
                                    'hash-eq?
                                    'hash-eqv?
                                    'hash
                                    'hasheqv
                                    'hasheq
                                    'make-hash
                                    'make-hasheqv
                                    'make-hasheq
                                    'make-immutable-hash
                                    'make-immutable-hasheqv
                                    'make-immutable-hasheq
                                    'hash-copy
                                    'hash-ref
                                    'hash-has-key?
                                    'hash-set!
                                    'hash-set
                                    'hash-remove!
                                    'hash-remove
                                    'equal-hash-code
                                    'hash-count
                                    'hash-keys
                                    'hash-values
                                    
                                    'string-copy

                                    'unsafe-car
                                    'unsafe-cdr

                                    'continuation-prompt-available?
                                    'abort-current-continuation
                                    'call-with-continuation-prompt
                                    ))
; (define-predicate KernelPrimitiveName? KernelPrimitiveName)
(define (KernelPrimitiveName? s)
  (member s KernelPrimitiveNames))



;; These are the primitives that we know how to inline.
(define KernelPrimitiveNames/Inline (list '+
                                           '-
                                           '*
                                           '/
                                           'zero?
                                           'add1
                                           'sub1
                                           '<
                                           '<=
                                           '=
                                           '>
                                           '>=
                                           'cons
                                           'car
                                           'caar
                                           'cdr
                                           'list
                                           'list?
                                           'pair?
                                           'null?
                                           'not
                                           'eq?
                                           'vector-ref
                                           'vector-set!
                                           ))

(ensure-type-subsetof KernelPrimitiveName/Inline KernelPrimitiveName)

; (define-predicate KernelPrimitiveName/Inline? KernelPrimitiveName/Inline)
(define (KernelPrimitiveName/Inline? s)
  (member s KernelPrimitiveNames/Inline))

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

    [(eq? prim 'zero?)
     (cond [(= arity 1)
            (list 'number)]
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

    [(eq? prim 'caar)
     (list 'caarpair)]
    
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
     (list 'any 'any)]

    [(eq? prim 'vector-ref)
     (list 'vector 'number)]

    [(eq? prim 'vector-set!)
     (list 'vector 'number 'any)]))
