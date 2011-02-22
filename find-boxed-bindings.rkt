#lang typed/racket/base
(require "typed-structs.rkt"
         "lexical-env.rkt"
         "find-toplevel-variables.rkt")


(: find-boxed-bindings (Expression -> (HashTable Expression Boolean)))
;; Collects the list of toplevel variables we need.
(define (find-boxed-bindings exp)
  
  (: ht (HashTable Expression Boolean))
  (define ht (make-hasheq))
  
  (: loop (Expression CompileTimeEnvironment -> 'ok))
  (define (loop exp cenv)
    (cond
      [(Constant? exp)
       'ok]
      
      [(Quote? exp)
       'ok]
      
      [(Var? exp)
       'ok]
      
      [(Assign? exp)
       (let ([lexical-address
              (find-variable (Assign-variable exp) cenv)])
         (cond
           [(LocalAddress? lexical-address)
            (hash-set! ht exp #t)
            'ok]
           [(PrefixAddress? lexical-address)
            'ok]))
       (loop (Assign-value exp) cenv)]
      
      [(Def? exp)
       (loop (Def-value exp) cenv)]
      
      [(Branch? exp)
       (loop (Branch-predicate exp) cenv)
       (loop (Branch-consequent exp) cenv)
       (loop (Branch-alternative exp) cenv)
       'ok]
      
      [(Lam? exp)
       (let ([extended-cenv
              (extend-lexical-environment cenv (Lam-parameters exp))])

         (for-each (lambda: ([e : Expression]) (loop e extended-cenv))
                   (Lam-body exp))
         'ok)]

      
      [(Seq? exp)
       (for-each (lambda: ([e : Expression]) (loop e cenv)) (Seq-actions exp))
       'ok]
      
      [(App? exp)
       (loop (App-operator exp) cenv)
       (for-each (lambda: ([e : Expression]) (loop e cenv)) (App-operands exp))
       'ok]))
  
  (let*: ([names : (Listof Symbol) (find-toplevel-variables exp)]
          [cenv : CompileTimeEnvironment (list (make-Prefix names))])
  
         (loop exp cenv))
  ht)