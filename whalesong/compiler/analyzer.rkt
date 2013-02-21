#lang typed/racket/base

(require "expression-structs.rkt"
         "analyzer-structs.rkt"
         "arity-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "compiler-structs.rkt"
         racket/list)

(require/typed "compiler-helper.rkt"
               [ensure-const-value (Any -> const-value)])


(provide collect-all-lambdas-with-bodies
         collect-lam-applications
         extract-static-knowledge
         ensure-prefix)

;; Holds helper functions we use for different analyses.

;; Given a lambda body, collect all the applications that exist within
;; it.  We'll use this to determine what procedures can safely be
;; transformed into primitives.
(: collect-lam-applications (Lam CompileTimeEnvironment -> (Listof CompileTimeEnvironmentEntry)))
(define (collect-lam-applications lam cenv)
  
  (let: loop : (Listof CompileTimeEnvironmentEntry)
    ([exp : Expression (Lam-body lam)]
     [cenv : CompileTimeEnvironment cenv]
     [acc : (Listof CompileTimeEnvironmentEntry) '()])
    
    (cond
      [(Top? exp)
       (loop (Top-code exp)
             (cons (Top-prefix exp) cenv)
             acc)]
      
      [(Module? exp)
       (loop (Module-code exp)
             (cons (Module-prefix exp) cenv)
             acc)]
      
      [(Constant? exp)
       acc]
      
      [(LocalRef? exp)
       acc]
      
      [(ToplevelRef? exp)
       acc]
      
      [(ToplevelSet? exp)
       (loop (ToplevelSet-value exp) cenv acc)]
      
      [(Branch? exp)
       (define acc-1 (loop (Branch-predicate exp) cenv acc))
       (define acc-2 (loop (Branch-consequent exp) cenv acc-1))
       (define acc-3 (loop (Branch-alternative exp) cenv acc-2))
       acc-3]
      
      [(Lam? exp)
       acc]
      
      [(CaseLam? exp)
       acc]
      
      [(EmptyClosureReference? exp)
       acc]
      
      [(Seq? exp)
       (foldl (lambda: ([e : Expression]
                        [acc : (Listof CompileTimeEnvironmentEntry)])
                (loop e cenv acc))
              acc
              (Seq-actions exp))]
      
      [(Splice? exp)
       (foldl (lambda: ([e : Expression]
                        [acc : (Listof CompileTimeEnvironmentEntry)])
                (loop e cenv acc))
              acc
              (Splice-actions exp))]
      
      [(Begin0? exp)
       (foldl (lambda: ([e : Expression]
                        [acc : (Listof CompileTimeEnvironmentEntry)])
                (loop e cenv acc))
              acc
              (Begin0-actions exp))]
      
      [(App? exp)
       (define new-cenv
         (append (build-list (length (App-operands exp)) (lambda: ([i : Natural]) '?))
                 cenv))
       (foldl (lambda: ([e : Expression]
                        [acc : (Listof CompileTimeEnvironmentEntry)])
                (loop e new-cenv acc))
              (cons (extract-static-knowledge (App-operator exp) new-cenv)
                    (loop (App-operator exp) new-cenv acc))
              (App-operands exp))]
      
      [(Let1? exp)
       (define acc-1 (loop (Let1-rhs exp) (cons '? cenv) acc))
       (define acc-2 (loop (Let1-body exp) 
                           (cons (extract-static-knowledge (Let1-rhs exp) (cons '? cenv)) 
                                 cenv)
                           acc-1))
       acc-2]
      
      [(LetVoid? exp)
       (loop (LetVoid-body exp) 
             (append (build-list (LetVoid-count exp) (lambda: ([i : Natural]) '?))
                     cenv)
             acc)]
      
      [(InstallValue? exp)
       (loop (InstallValue-body exp) cenv acc)]
      
      [(BoxEnv? exp)
       (loop (BoxEnv-body exp) cenv acc)]
      
      [(LetRec? exp)
       (let ([n (length (LetRec-procs exp))])
         (let ([new-cenv (append (map (lambda: ([p : Lam]) 
                                        (extract-static-knowledge 
                                         p 
                                         (append (build-list (length (LetRec-procs exp))
                                                             (lambda: ([i : Natural]) '?))
                                                 (drop cenv n))))
                                      (LetRec-procs exp))
                                 (drop cenv n))])
           (loop (LetRec-body exp) new-cenv acc)))]
      
      [(WithContMark? exp)
       (define acc-1 (loop (WithContMark-key exp) cenv acc))
       (define acc-2 (loop (WithContMark-value exp) cenv acc-1))
       (define acc-3 (loop (WithContMark-body exp) cenv acc-2))
       acc-3]
      
      [(ApplyValues? exp)
       (define acc-1 (loop (ApplyValues-proc exp) cenv acc))
       (define acc-2 (loop (ApplyValues-args-expr exp) cenv acc-1))
       acc-2]
      
      [(DefValues? exp)
       (loop (DefValues-rhs exp) cenv acc)]
      
      [(PrimitiveKernelValue? exp)
       acc]
      
      [(VariableReference? exp)
       (loop (VariableReference-toplevel exp) cenv acc)]
      
      [(Require? exp)
       acc])))





(: extract-static-knowledge (Expression CompileTimeEnvironment ->  
                                        CompileTimeEnvironmentEntry))
;; Statically determines what we know about the expression, given the compile time environment.
;; We should do more here eventually, including things like type inference or flow analysis, so that
;; we can generate better code.
(define (extract-static-knowledge exp cenv)
  (cond
    [(Lam? exp)
     ;(log-debug "known to be a lambda")
     (make-StaticallyKnownLam (Lam-name exp)
                              (Lam-entry-label exp)
                              (if (Lam-rest? exp)
                                  (make-ArityAtLeast (Lam-num-parameters exp))
                                  (Lam-num-parameters exp)))]
    [(and (LocalRef? exp) 
          (not (LocalRef-unbox? exp)))
     (let ([entry (list-ref cenv (LocalRef-depth exp))])
       ;(log-debug (format "known to be ~s" entry))
       entry)]
    
    [(EmptyClosureReference? exp)
     (make-StaticallyKnownLam (EmptyClosureReference-name exp)
                              (EmptyClosureReference-entry-label exp)
                              (if (EmptyClosureReference-rest? exp)
                                  (make-ArityAtLeast (EmptyClosureReference-num-parameters exp))
                                  (EmptyClosureReference-num-parameters exp)))]
    [(ToplevelRef? exp)
     ;(log-debug (format "toplevel reference of ~a" exp))
     ;(when (ToplevelRef-constant? exp)
     ;  (log-debug (format "toplevel reference ~a should be known constant" exp)))
     (let: ([name : (U Symbol False GlobalBucket ModuleVariable)
                  (list-ref (Prefix-names (ensure-prefix (list-ref cenv (ToplevelRef-depth exp))))
                            (ToplevelRef-pos exp))])
       (cond
         [(ModuleVariable? name)
          ;(log-debug (format "toplevel reference is to ~s" name))
          name]
         [(GlobalBucket? name)
          '?]
         [else
          ;(log-debug (format "nothing statically known about ~s" exp))
          '?]))]
    
    [(Constant? exp)
     (make-Const (ensure-const-value (Constant-v exp)))]
    
    [(PrimitiveKernelValue? exp)
     exp]
    
    [else
     ;(log-debug (format "nothing statically known about ~s" exp))
     '?]))









(: collect-all-lambdas-with-bodies (Expression -> (Listof lam+cenv)))
;; Finds all the lambdas in the expression.
(define (collect-all-lambdas-with-bodies exp)
  (let: loop : (Listof lam+cenv)
    ([exp : Expression exp]
     [cenv : CompileTimeEnvironment '()])
    
    (cond
      [(Top? exp)
       (loop (Top-code exp) (cons (Top-prefix exp) cenv))]
      [(Module? exp)
       (loop (Module-code exp) (cons (Module-prefix exp) cenv))]
      [(Constant? exp)
       '()]
      [(LocalRef? exp)
       '()]
      [(ToplevelRef? exp)
       '()]
      [(ToplevelSet? exp)
       (loop (ToplevelSet-value exp) cenv)]
      [(Branch? exp)
       (append (loop (Branch-predicate exp) cenv)
               (loop (Branch-consequent exp) cenv)
               (loop (Branch-alternative exp) cenv))]
      [(Lam? exp)
       (cons (make-lam+cenv exp (extract-lambda-cenv exp cenv))
             (loop (Lam-body exp) 
                   (extract-lambda-cenv exp cenv)))]
      [(CaseLam? exp)
       (cons (make-lam+cenv exp cenv)
             (apply append (map (lambda: ([lam : (U Lam EmptyClosureReference)])
                                  (loop lam cenv))
                                (CaseLam-clauses exp))))]
      
      [(EmptyClosureReference? exp)
       '()]
      
      [(Seq? exp)
       (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                          (Seq-actions exp)))]
      [(Splice? exp)
       (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                          (Splice-actions exp)))]
      [(Begin0? exp)
       (apply append (map (lambda: ([e : Expression]) (loop e cenv))
                          (Begin0-actions exp)))]
      [(App? exp)
       (let ([new-cenv (append (build-list (length (App-operands exp)) (lambda: ([i : Natural]) '?))
                               cenv)])
         (append (loop (App-operator exp) new-cenv)
                 (apply append (map (lambda: ([e : Expression]) (loop e new-cenv)) (App-operands exp)))))]
      [(Let1? exp)
       (append (loop (Let1-rhs exp)
                     (cons '? cenv))
               (loop (Let1-body exp) 
                     (cons (extract-static-knowledge (Let1-rhs exp) (cons '? cenv)) 
                           cenv)))]
      [(LetVoid? exp)
       (loop (LetVoid-body exp) 
             (append (build-list (LetVoid-count exp) (lambda: ([i : Natural]) '?))
                     cenv))]
      [(InstallValue? exp)
       (loop (InstallValue-body exp) cenv)]
      [(BoxEnv? exp)
       (loop (BoxEnv-body exp) cenv)]
      [(LetRec? exp)
       (let ([n (length (LetRec-procs exp))])
         (let ([new-cenv (append (map (lambda: ([p : Lam]) 
                                        (extract-static-knowledge 
                                         p 
                                         (append (build-list (length (LetRec-procs exp))
                                                             (lambda: ([i : Natural]) '?))
                                                 (drop cenv n))))
                                      (LetRec-procs exp))
                                 (drop cenv n))])
           (append (apply append 
                          (map (lambda: ([lam : Lam])
                                 (loop lam new-cenv))
                               (LetRec-procs exp)))
                   (loop (LetRec-body exp) new-cenv))))]
      [(WithContMark? exp)
       (append (loop (WithContMark-key exp) cenv)
               (loop (WithContMark-value exp) cenv)
               (loop (WithContMark-body exp) cenv))]
      [(ApplyValues? exp)
       (append (loop (ApplyValues-proc exp) cenv)
               (loop (ApplyValues-args-expr exp) cenv))]
      [(DefValues? exp)
       (append (loop (DefValues-rhs exp) cenv))]
      [(PrimitiveKernelValue? exp)
       '()]
      [(VariableReference? exp)
       (loop (VariableReference-toplevel exp) cenv)]
      [(Require? exp)
       '()])))



(: extract-lambda-cenv (Lam CompileTimeEnvironment -> CompileTimeEnvironment))
;; Given a Lam and the ambient environment, produces the compile time environment for the
;; body of the lambda.
(define (extract-lambda-cenv lam cenv)
  (append (map (lambda: ([d : Natural])
                 (list-ref cenv d))
               (Lam-closure-map lam))
          (build-list (if (Lam-rest? lam)
                          (add1 (Lam-num-parameters lam))
                          (Lam-num-parameters lam))
                      (lambda: ([i : Natural]) '?))))










(: ensure-prefix (CompileTimeEnvironmentEntry -> Prefix))
(define (ensure-prefix x)
  (if (Prefix? x)
      x
      (error 'ensure-prefix "Not a prefix: ~s" x)))
