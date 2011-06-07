#lang typed/racket/base

(require "assemble-helpers.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/kernel-primitives.rkt"
         racket/string
         racket/list
         typed/rackunit)

(provide open-code-kernel-primitive-procedure)



(: open-code-kernel-primitive-procedure (CallKernelPrimitiveProcedure -> String))
(define (open-code-kernel-primitive-procedure op)
  (let*: ([operator : KernelPrimitiveName/Inline (CallKernelPrimitiveProcedure-operator op)]
          [operands : (Listof String) (map assemble-oparg (CallKernelPrimitiveProcedure-operands op))]
          [checked-operands : (Listof String)
                            (map (lambda: ([dom : OperandDomain]
					   [pos : Natural]
					   [rand : String]
					   [typecheck? : Boolean])
				   (maybe-typecheck-operand operator dom pos rand typecheck?))
                                 (CallKernelPrimitiveProcedure-expected-operand-types op)
                                 (build-list (length operands) (lambda: ([i : Natural]) i))
                                 operands
                                 (CallKernelPrimitiveProcedure-typechecks? op))])
        (case operator
          [(+)
           (cond [(empty? checked-operands)
                  (assemble-numeric-constant 0)]
                 [else
                  (assemble-binop-chain "jsnums.add" checked-operands)])]
          
          [(-)
           (cond [(empty? (rest checked-operands))
                  (assemble-binop-chain "jsnums.subtract" (cons "0" checked-operands))]
                 [else
                  (assemble-binop-chain "jsnums.subtract" checked-operands)])]
          
          [(*)
           (cond [(empty? checked-operands)
                  (assemble-numeric-constant 1)]
                 [else
                  (assemble-binop-chain "jsnums.multiply" checked-operands)])]

          [(/)
           (assemble-binop-chain "jsnums.divide" checked-operands)]

          [(add1)
           (assemble-binop-chain "jsnums.add" (cons "1" checked-operands))]
          
          [(sub1)
           (assemble-binop-chain "jsnums.subtract" (append checked-operands (list "1")))]

          [(<)
           (assemble-boolean-chain "jsnums.lessThan" checked-operands)]

          [(<=)
           (assemble-boolean-chain "jsnums.lessThanOrEqual" checked-operands)]
          
          [(=)
           (assemble-boolean-chain "jsnums.equals" checked-operands)]
          
          [(>)
           (assemble-boolean-chain "jsnums.greaterThan" checked-operands)]
          
          [(>=)
           (assemble-boolean-chain "jsnums.greaterThanOrEqual" checked-operands)]
          
          [(cons)
           (format "RUNTIME.makePair(~a, ~a)"
                   (first checked-operands)
                   (second checked-operands))]

          [(car)
           (format "(~a).first" (first checked-operands))]
          
          [(cdr)
           (format "(~a).rest" (first checked-operands))]
          
          [(list)
           (let loop ([checked-operands checked-operands])
             (assemble-listof-assembled-values checked-operands))]
          
          [(null?)
           (format "(~a === RUNTIME.NULL)" (first checked-operands))]

          [(not)
           (format "(~a === false)" (first checked-operands))]
          
          [(eq?)
           (format "(~a === ~a)" (first checked-operands) (second checked-operands))])))



(: assemble-binop-chain (String (Listof String) -> String))
(define (assemble-binop-chain rator rands)
  (cond
   [(empty? rands)
    ""]
   [(empty? (rest rands))
    (first rands)]
   [else
    (assemble-binop-chain
     rator
     (cons (string-append rator "(" (first rands) ", " (second rands) ")")
           (rest (rest rands))))]))

(check-equal? (assemble-binop-chain "jsnums.add" '("3" "4" "5"))
              "jsnums.add(jsnums.add(3, 4), 5)")
(check-equal? (assemble-binop-chain "jsnums.subtract" '("0" "42"))
              "jsnums.subtract(0, 42)")






(: assemble-boolean-chain (String (Listof String) -> String))
(define (assemble-boolean-chain rator rands)
  (string-append "("
                 (string-join (let: loop : (Listof String) ([rands : (Listof String) rands])
                                (cond
                                  [(empty? rands)
                                   '()]
                                  [(empty? (rest rands))
                                   '()]
                                  [else
                                   (cons (format "(~a(~a,~a))" rator (first rands) (second rands))
                                         (loop (rest rands)))]))
                              "&&")
                 ")"))





(: assemble-domain-check (Symbol OperandDomain String Natural -> String))
(define (assemble-domain-check caller domain operand-string pos)
  (cond
    [(eq? domain 'any)
     operand-string]
    [else
     (let: ([test-string : String
                         (case domain
                           [(number)
                            (format "jsnums.isSchemeNumber(~a)"
                                    operand-string)]             
                           [(string)
                            (format "(typeof(~a) === 'string')"
                                    operand-string)]
                           [(list)
                            (format "RUNTIME.isList(~a)" operand-string)]
                           [(pair)
                            (format "RUNTIME.isPair(~a)" operand-string)]
                           [(box)
                            (format "(typeof(~a) === 'object' && (~a).length === 1)"
                                    operand-string operand-string)])])
           (format "((~a) ? (~a) : RUNTIME.raiseArgumentTypeError(MACHINE, ~s, ~s, ~s, ~a))"
                   test-string
                   operand-string
		   (symbol->string caller)
                   (symbol->string domain)
                   pos
                   operand-string))]))


(: maybe-typecheck-operand (Symbol OperandDomain Natural String Boolean -> String))
;; Adds typechecks if we can't prove that the operand is of the required type.
(define (maybe-typecheck-operand caller domain-type position operand-string typecheck?)
  (cond
    [typecheck?
     (assemble-domain-check caller domain-type operand-string position)]
    [else
     operand-string]))
