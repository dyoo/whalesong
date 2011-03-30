#lang typed/racket/base

(require "il-structs.rkt"
         "lexical-structs.rkt"
         "assemble-helpers.rkt"
         racket/string
         racket/list)

(provide open-code-kernel-primitive-procedure)



(: open-code-kernel-primitive-procedure (CallKernelPrimitiveProcedure -> String))
(define (open-code-kernel-primitive-procedure op)
  (let: ([operator : KernelPrimitiveName (CallKernelPrimitiveProcedure-operator op)]
         [rand-knowledge : (Listof CompileTimeEnvironmentEntry)
                        (CallKernelPrimitiveProcedure-operands-knowledge op)]
         [rand-vals : (Listof String) (map assemble-input (CallKernelPrimitiveProcedure-operands op))])
        (case operator
          
          [(+)
           (let ([checked-rands (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                          rand-vals 
                                                          rand-knowledge)])
                 (cond [(empty? rand-vals)
                        "0"]
                       [else
                        (string-append "(" (string-join checked-rands " + ") ")")]))]
          
          [(-)
           (let ([checked-rands (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                          rand-vals 
                                                          rand-knowledge)])
             (cond [(empty? rand-vals)
                    (error '- "Expects at least 1 argument, given 0")]
                   [(empty? (rest rand-vals))
                    (format "(-(~a))" (first rand-vals))]
                   [else
                    (string-append "(" (string-join checked-rands "-") ")")]))]

          [(*)
           (let ([checked-rands (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                          rand-vals 
                                                          rand-knowledge)])
             (cond [(empty? rand-vals)
                    "1"]
                   [else
                    (string-append "(" (string-join checked-rands "*") ")")]))]

          [(/)
           (let ([checked-rands (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                          rand-vals 
                                                          rand-knowledge)])
             (cond [(empty? rand-vals)
                    (error '/ "Expects at least 1 argument, given 0")]
                   [else
                    (string-append "(" (string-join checked-rands "/") ")")]))]

          [(add1)
           (unless (= 1 (length rand-vals))
             (error 'add1 "Expected one argument"))
           (format "(~a + 1)" 
                   (maybe-typecheck-operand 'number 0 (first rand-vals) (first rand-knowledge)))]
          
          [(sub1)
           (unless (= 1 (length rand-vals))
             (error 'sub1 "Expected one argument"))
           (format "(~a - 1)" 
                   (maybe-typecheck-operand 'number 0 (first rand-vals) (first rand-knowledge)))]
          
          [(<)
           (unless (> (length rand-vals) 0)
             (error '< "Expected at least one argument"))
           (assemble-chain "<" (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                         rand-vals
                                                         rand-knowledge))]
          [(<=)
           (unless (> (length rand-vals) 0)
             (error '<= "Expected at least one argument"))
           (assemble-chain "<=" (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                         rand-vals
                                                         rand-knowledge))]
          
          [(=)
           (unless (> (length rand-vals) 0)
             (error '= "Expected at least one argument"))
           (assemble-chain "==" (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                         rand-vals
                                                         rand-knowledge))]
          
          [(>)
           (unless (> (length rand-vals) 0)
             (error '> "Expected at least one argument"))
           (assemble-chain ">" (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                         rand-vals
                                                         rand-knowledge))]
          
          [(>=)
           (unless (> (length rand-vals) 0)
             (error '>= "Expected at least one argument"))
           (assemble-chain ">=" (maybe-typecheck-operands (repeat 'number (length rand-vals))
                                                         rand-vals
                                                         rand-knowledge))]
          
          [(cons)
           (unless (= (length rand-vals) 2)
             (error 'cons "Expected two arguments"))
           (format "[~a, ~a]" (first rand-vals) (second rand-vals))]

          [(car)
           (unless (= (length rand-vals) 1)
             (error 'car "Expected one argument"))
           (format "(~a)[0]" (maybe-typecheck-operand 'pair 0 (first rand-vals)
                                                      (first rand-knowledge)))]
          
          [(cdr)
           (unless (= (length rand-vals) 1)
             (error 'cdr "Expected one argument"))
           (format "(~a)[1]" (maybe-typecheck-operand 'pair 0 (first rand-vals)
                                                      (first rand-knowledge)))]
          
          [(list)
           (let loop ([rand-vals rand-vals])
             (cond
               [(empty? rand-vals)
                "Primitives.null"]
               [else
                (format "[~a,~a]" (first rand-vals) (loop (rest rand-vals)))]))]
          
          [(null?)
           (unless (= (length rand-vals) 1)
             (error 'null? "Expected one argument"))
           (format "(~a === Primitives.null)"
                   (first rand-vals))]
          [(not)
           (unless (= (length rand-vals) 1)
             (error 'not? "Expected one argument"))
           (format "(!~a)" (first rand-vals))]
          
          [(eq?)
           (unless (= (length rand-vals) 2)
             (error 'eq? "Expected 2 arguments"))
           (format "(~a === ~a)" (first rand-vals) (second rand-vals))])))


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



(define-type OperandDomain (U 'number
                              'string
                              'box
                              'list
                              'pair))


(: assemble-domain-check (OperandDomain String Natural -> String))
(define (assemble-domain-check domain operand-string pos)
  (let ([test-string 
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
                    operand-string operand-string)])])
    (format "((~a) ? (~a) : raise(new Error('Expected ' + ~s + ' as argument ' + ~s + ' but received ' + ~a)))"
            test-string
            operand-string
            (symbol->string domain)
            pos
            operand-string)))

(: maybe-typecheck-operands ((Listof OperandDomain) 
                             (Listof String) 
                             (Listof CompileTimeEnvironmentEntry) -> (Listof String)))
(define (maybe-typecheck-operands expected-domains rand-vals rand-knowledge)
  (map (lambda: ([rand : String]
                 [expected-domain : OperandDomain]
                 [knowledge : CompileTimeEnvironmentEntry]
                 [position : Natural])
                (maybe-typecheck-operand expected-domain position rand knowledge))
       rand-vals
       expected-domains
       rand-knowledge
       (build-list (length rand-vals)
                   (lambda: ([i : Natural]) i))))


(: maybe-typecheck-operand (OperandDomain Natural String CompileTimeEnvironmentEntry -> String))
;; Adds typechecks if we can't prove that the operand is of the required type.
(define (maybe-typecheck-operand domain-type position operand-string knowledge)
  (cond
    [(redundant-check? domain-type knowledge)
     operand-string]
    [else
     (assemble-domain-check domain-type operand-string position)]))


(: redundant-check? (OperandDomain CompileTimeEnvironmentEntry -> Boolean))
;; Produces true if we know the knowledge implies the domain-type.
(define (redundant-check? domain-type knowledge)
  (cond [(Const? knowledge)
         (case domain-type
           [(number)
            (number? (Const-const knowledge))]
           [(string)
            (string? (Const-const knowledge))]
           [(box)
            (box? (Const-const knowledge))]
           [(list)
            (list? (Const-const knowledge))]
           [(pair)
            (pair? (Const-const knowledge))])]
        [else
         #f]))


(: repeat (All (A) (A Natural -> (Listof A))))
(define (repeat x n)
  (build-list n (lambda (i) x)))