#lang typed/racket/base
(require "typed-structs.rkt"
         "helpers.rkt"
         racket/list)

;; Collects the list of toplevel variables we need.

(: find-toplevel (Expression -> (Listof Symbol)))
(define (find-toplevel exp)
  (cond
    [(Constant? exp)
     empty]
    
    [(Quote? exp)
     empty]
    
    [(Var? exp)
     (list (Var-id exp))]
    
    [(Assign? exp)
     (find-toplevel (Assign-value exp))]
    
    [(Def? exp)
     (find-toplevel (Def-value exp))]

    [(Branch? exp)
     (append (find-toplevel (Branch-predicate exp))
             (find-toplevel (Branch-consequent exp))
             (find-toplevel (Branch-alternative exp)))]
    
    [(Lam? exp)
     (list-difference (apply append (map find-toplevel (Lam-body exp)))
                      (Lam-parameters exp))]
    [(Seq? exp)
     (apply append (map find-toplevel (Seq-actions exp)))]

    [(App? exp)
     (append (find-toplevel (App-operator exp))
             (apply append (map find-toplevel (App-operands exp))))]))
