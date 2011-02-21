#lang typed/racket/base
(require "typed-structs.rkt"
         "helpers.rkt"
         racket/list)

(provide find-toplevel-variables)


(: find-toplevel-variables (Expression -> (Listof Symbol)))
;; Collects the list of toplevel variables we need.
(define (find-toplevel-variables exp)
  (: loop (Expression -> (Listof Symbol)))
  (define (loop exp)
    (cond
      [(Constant? exp)
       empty]
      
      [(Quote? exp)
       empty]
      
      [(Var? exp)
       (list (Var-id exp))]
      
      [(Assign? exp)
       (loop (Assign-value exp))]
      
      [(Def? exp)
       (cons (Def-variable exp)
             (loop (Def-value exp)))]
      
      [(Branch? exp)
       (append (loop (Branch-predicate exp))
               (loop (Branch-consequent exp))
               (loop (Branch-alternative exp)))]
      
      [(Lam? exp)
       (list-difference (apply append (map loop (Lam-body exp)))
                        (Lam-parameters exp))]
      [(Seq? exp)
       (apply append (map loop (Seq-actions exp)))]
      
      [(App? exp)
       (append (loop (App-operator exp))
               (apply append (map loop (App-operands exp))))]))
  
  (unique (loop exp)))
  