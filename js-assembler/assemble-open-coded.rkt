#lang typed/racket/base

(require "assemble-helpers.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/kernel-primitives.rkt"
         racket/string
         racket/list)

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
                  "0"]
                 [else
                  (string-append "(" (string-join checked-operands " + ") ")")])]
          
          [(-)
           (cond [(empty? (rest checked-operands))
                  (format "(-(~a))" (first checked-operands))]
                 [else
                  (string-append "(" (string-join checked-operands "-") ")")])]
          
          [(*)
           (cond [(empty? checked-operands)
                  "1"]
                 [else
                  (string-append "(" (string-join checked-operands "*") ")")])]

          [(/)
           (string-append "(" (string-join checked-operands "/") ")")]

          [(add1)
           (format "(~a + 1)" (first checked-operands))]
          
          [(sub1)
           (format "(~a - 1)" (first checked-operands))]
          
          [(<)
           (assemble-chain "<" checked-operands)]

          [(<=)
           (assemble-chain "<=" checked-operands)]
          
          [(=)
           (assemble-chain "===" checked-operands)]
          
          [(>)
           (assemble-chain ">" checked-operands)]
          
          [(>=)
           (assemble-chain ">=" checked-operands)]
          
          [(cons)
           (format "[~a, ~a]" (first checked-operands) (second checked-operands))]

          [(car)
           (format "(~a)[0]" (first checked-operands))]
          
          [(cdr)
           (format "(~a)[1]" (first checked-operands))]
          
          [(list)
           (let loop ([checked-operands checked-operands])
             (cond
               [(empty? checked-operands)
                "RUNTIME.NULL"]
               [else
                (format "[~a,~a]" (first checked-operands) (loop (rest checked-operands)))]))]
          
          [(null?)
           (format "(~a === RUNTIME.NULL)" (first checked-operands))]

          [(not)
           (format "(~a === false)" (first checked-operands))]
          
          [(eq?)
           (format "(~a === ~a)" (first checked-operands) (second checked-operands))])))


(: assemble-chain (String (Listof String) -> String))
(define (assemble-chain rator rands)
  (string-append "("
                 (string-join (let: loop : (Listof String) ([rands : (Listof String) rands])
                                (cond
                                  [(empty? rands)
                                   '()]
                                  [(empty? (rest rands))
                                   '()]
                                  [else
                                   (cons (format "(~a ~a ~a)" (first rands) rator (second rands))
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
                            (format "(typeof(~a) === 'number')"
                                    operand-string)]             
                           [(string)
                            (format "(typeof(~a) === 'string')"
                                    operand-string)]
                           [(list)
                            (format "(~a === [] || (typeof(~a) === 'object' && (~a).length === 2))"
                                    operand-string operand-string operand-string)]
                           [(pair)
                            (format "(typeof(~a) === 'object' && (~a).length === 2)"
                                    operand-string operand-string)]
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
