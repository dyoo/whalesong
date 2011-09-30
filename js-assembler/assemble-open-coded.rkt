#lang typed/racket/base

(require "assemble-helpers.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/kernel-primitives.rkt"
         "assemble-structs.rkt"
         racket/string
         racket/list
         typed/rackunit)

(provide open-code-kernel-primitive-procedure)



(: open-code-kernel-primitive-procedure (CallKernelPrimitiveProcedure Blockht -> String))
(define (open-code-kernel-primitive-procedure op blockht)
  (let*: ([operator : KernelPrimitiveName/Inline (CallKernelPrimitiveProcedure-operator op)]
          [operands : (Listof String) (map (lambda: ([op : OpArg])
                                                    (assemble-oparg op blockht))
                                           (CallKernelPrimitiveProcedure-operands op))]
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
                  (assemble-binop-chain "plt.baselib.numbers.add" checked-operands)])]
          
          [(-)
           (cond [(empty? (rest checked-operands))
                  (assemble-binop-chain "plt.baselib.numbers.subtract" (cons "0" checked-operands))]
                 [else
                  (assemble-binop-chain "plt.baselib.numbers.subtract" checked-operands)])]
          
          [(*)
           (cond [(empty? checked-operands)
                  (assemble-numeric-constant 1)]
                 [else
                  (assemble-binop-chain "plt.baselib.numbers.multiply" checked-operands)])]

          [(/)
           (assemble-binop-chain "plt.baselib.numbers.divide" checked-operands)]

          [(add1)
           (assemble-binop-chain "plt.baselib.numbers.add" (cons "1" checked-operands))]
          
          [(sub1)
           (assemble-binop-chain "plt.baselib.numbers.subtract" (append checked-operands (list "1")))]

          [(<)
           (assemble-boolean-chain "plt.baselib.numbers.lessThan" checked-operands)]

          [(<=)
           (assemble-boolean-chain "plt.baselib.numbers.lessThanOrEqual" checked-operands)]
          
          [(=)
           (assemble-boolean-chain "plt.baselib.numbers.equals" checked-operands)]
          
          [(>)
           (assemble-boolean-chain "plt.baselib.numbers.greaterThan" checked-operands)]
          
          [(>=)
           (assemble-boolean-chain "plt.baselib.numbers.greaterThanOrEqual" checked-operands)]
          
          [(cons)
           (format "RT.makePair(~a,~a)"
                   (first checked-operands)
                   (second checked-operands))]

          [(car)
           (format "(~a).first" (first checked-operands))]

          [(caar)
           (format "(~a).first.first" (first checked-operands))]
          
          [(cdr)
           (format "(~a).rest" (first checked-operands))]
          
          [(list)
           (let loop ([checked-operands checked-operands])
             (assemble-listof-assembled-values checked-operands))]

          [(list?)
           (format "RT.isList(~a)"
                   (first checked-operands))]
          
          [(pair?)
           (format "RT.isPair(~a)"
                   (first checked-operands))]
          
          [(null?)
           (format "(~a===RT.NULL)" (first checked-operands))]

          [(not)
           (format "(~a===false)" (first checked-operands))]
          
          [(eq?)
           (format "(~a===~a)" (first checked-operands) (second checked-operands))])))



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

(check-equal? (assemble-binop-chain "plt.baselib.numbers.add" '("3" "4" "5"))
              "plt.baselib.numbers.add(plt.baselib.numbers.add(3, 4), 5)")
(check-equal? (assemble-binop-chain "plt.baselib.numbers.subtract" '("0" "42"))
              "plt.baselib.numbers.subtract(0, 42)")






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
     (let: ([predicate : String
                         (case domain
                           [(number)
                            (format "RT.isNumber")]
                           [(string)
                            (format "RT.isString")]
                           [(list)
                            (format "RT.isList")]
                           [(pair)
                            (format "RT.isPair")]
                           [(caarpair)
                            (format "RT.isCaarPair")]
                           [(box)
                            (format "RT.isBox")])])
           (format "RT.testArgument(M,~s,~a,~a,~a,~s)"
                   (symbol->string domain)
                   predicate
                   operand-string
                   pos
                   (symbol->string caller)))]))


(: maybe-typecheck-operand (Symbol OperandDomain Natural String Boolean -> String))
;; Adds typechecks if we can't prove that the operand is of the required type.
(define (maybe-typecheck-operand caller domain-type position operand-string typecheck?)
  (cond
    [typecheck?
     (assemble-domain-check caller domain-type operand-string position)]
    [else
     operand-string]))
