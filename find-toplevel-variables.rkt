#lang typed/racket/base
(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "helpers.rkt"
         racket/list)

(provide find-toplevel-variables)


(: find-toplevel-variables (ExpressionCore -> (Listof Symbol)))
;; Collects the list of toplevel variables we need.
(define (find-toplevel-variables exp)
  (: loop (ExpressionCore -> (Listof Symbol)))
  (define (loop exp)
    (cond
      [(Top? exp)
       (list-difference (loop (Top-code exp))
                        (filter symbol? (Prefix-names (Top-prefix exp))))]
      [(Constant? exp)
       empty]
            
      [(Var? exp)
       (list (Var-id exp))]
            
      [(Def? exp)
       (cons (Def-variable exp)
             (loop (Def-value exp)))]
      
      [(Branch? exp)
       (append (loop (Branch-predicate exp))
               (loop (Branch-consequent exp))
               (loop (Branch-alternative exp)))]
      
      [(Lam? exp)
       (list-difference (loop (Lam-body exp))
                        (Lam-parameters exp))]
      [(Seq? exp)
       (apply append (map loop (Seq-actions exp)))]
      
      [(App? exp)
       (append (loop (App-operator exp))
               (apply append (map loop (App-operands exp))))]
      
      [(Let1? exp)
       (append (loop (Let1-rhs exp))
               (list-difference (loop (Let1-body exp))
                                (list (Let1-name exp))))]

      [(Let? exp)
       (append (apply append (map loop (Let-rhss exp)))
               (list-difference (loop (Let-body exp))
                                (Let-names exp)))]
       [(LetRec? exp)
        (append (apply append (map (lambda: ([rhs : ExpressionCore])
                                     (list-difference (loop rhs)
                                                      (LetRec-names exp)))
                                   (LetRec-rhss exp)))
                (list-difference (loop (LetRec-body exp))
                                 (LetRec-names exp)))]
       
      #;[(Letrec? exp)
       (list-difference (append (apply append (map loop (Letrec-procs exp)))
                                (loop (Letrec-body exp)))
                        (Letrec-names exp))]))
  
  (unique/eq? (loop exp)))
  