#lang typed/racket/base

(provide (rename-out [-analyze analyze])
         analysis-lookup
         analysis-alias!)

(require "analyzer-structs.rkt"
	 "expression-structs.rkt"
	 "il-structs.rkt"
	 "lexical-structs.rkt"
	 racket/match
	 racket/list)




(: current-expression-map
   (Parameterof (HashTable Expression CompileTimeEnvironmentEntry)))
(define current-expression-map (make-parameter 
				((inst make-hasheq Expression
				       CompileTimeEnvironmentEntry))))




(: -analyze (Expression -> Analysis))
(define (-analyze exp)
  (parameterize ([current-expression-map 
                  ((inst make-hasheq Expression CompileTimeEnvironmentEntry))])
    (analyze exp '())
    (make-Analysis (current-expression-map))))







(: analyze (Expression CompileTimeEnvironment -> 'ok))
;; Finds all the lambdas in the expression.
(define (analyze exp cenv)
  (cond
   [(Top? exp)
    (analyze-Top exp cenv)]
   [(Module? exp)
    (analyze-Module exp cenv)]
   [(Constant? exp)
    (analyze-Constant exp cenv)]
   [(LocalRef? exp)
    (analyze-LocalRef exp cenv)]
   [(ToplevelRef? exp)
    (analyze-ToplevelRef exp cenv)]
   [(ToplevelSet? exp)
    (analyze-ToplevelSet exp cenv)]
   [(Branch? exp)
    (analyze-Branch exp cenv)]
   [(Lam? exp)
    (analyze-Lam exp cenv)]
   [(CaseLam? exp)
    (analyze-CaseLam exp cenv)]
   [(EmptyClosureReference? exp)
    (analyze-EmptyClosureReference exp cenv)]
   [(Seq? exp)
    (analyze-Seq exp cenv)]
   [(Splice? exp)
    (analyze-Splice exp cenv)]
   [(Begin0? exp)
    (analyze-Begin0 exp cenv)]
   [(App? exp)
    (analyze-App exp cenv)]
   [(Let1? exp)
    (analyze-Let1 exp cenv)]
   [(LetVoid? exp)
    (analyze-LetVoid exp cenv)]
   [(InstallValue? exp)
    (analyze-InstallValue exp cenv)]
   [(BoxEnv? exp)
    (analyze-BoxEnv exp cenv)]
   [(LetRec? exp)
    (analyze-LetRec exp cenv)]
   [(WithContMark? exp)
    (analyze-WithContMark exp cenv)]
   [(ApplyValues? exp)
    (analyze-ApplyValues exp cenv)]
   [(DefValues? exp)
    (analyze-DefValues exp cenv)]
   [(PrimitiveKernelValue? exp)
    (analyze-PrimitiveKernelValue exp cenv)]
   [(VariableReference? exp)
    (analyze-VariableReference exp cenv)]
   [(Require? exp)
    (analyze-Require exp cenv)]))




(: analyze-Top (Top CompileTimeEnvironment -> 'ok))
(define (analyze-Top exp cenv)
  (match exp
	 [(struct Top (prefix code))
	  (analyze code (list prefix))]))


(: analyze-Module (Module CompileTimeEnvironment -> 'ok))
(define (analyze-Module exp cenv)
  (match exp
	 [(struct Module (name path prefix requires code))
	  (analyze code (list prefix))]))


(: analyze-Constant (Constant CompileTimeEnvironment -> 'ok))
(define (analyze-Constant exp cenv)
  'ok)


(: analyze-LocalRef (LocalRef CompileTimeEnvironment -> 'ok))
(define (analyze-LocalRef exp cenv)
  (annotate exp (extract-static-knowledge exp cenv))
  'ok)


(: analyze-ToplevelRef (ToplevelRef CompileTimeEnvironment -> 'ok))
(define (analyze-ToplevelRef exp cenv)
  (annotate exp (extract-static-knowledge exp cenv))
  'ok)


(: analyze-ToplevelSet (ToplevelSet CompileTimeEnvironment -> 'ok))
(define (analyze-ToplevelSet exp cenv)
  (match exp
	 [(struct ToplevelSet (depth pos value))
	  (analyze value cenv)]))


(: analyze-Branch (Branch CompileTimeEnvironment -> 'ok))
(define (analyze-Branch exp cenv)
  (match exp
	 [(struct Branch (test cons alter))
	  (analyze test cenv)
	  (analyze cons cenv)
	  (analyze alter cenv)]))


(: analyze-Lam (Lam CompileTimeEnvironment -> 'ok))
(define (analyze-Lam exp cenv)
  (match exp
	 [(struct Lam (name num-parameters rest? body closure-map entry-label))
	  (analyze body (extract-lambda-body-cenv exp cenv))]))



(: extract-lambda-body-cenv (Lam CompileTimeEnvironment -> CompileTimeEnvironment))
;; Given a Lam and the ambient environment, produces the compile time environment for the
;; body of the lambda.
(define (extract-lambda-body-cenv lam cenv)
  (append (map (lambda: ([d : Natural])
                        (list-ref cenv d))
               (Lam-closure-map lam))
          (build-list (if (Lam-rest? lam)
                          (add1 (Lam-num-parameters lam))
                          (Lam-num-parameters lam))
                      (lambda: ([i : Natural]) '?))))



(: analyze-CaseLam (CaseLam CompileTimeEnvironment -> 'ok))
(define (analyze-CaseLam exp cenv)
  (match exp
	 [(struct CaseLam (name clauses entry-label))
	  (for-each (lambda: ([c : Expression])
			     (analyze c cenv))
		    clauses)
	  'ok]))


(: analyze-EmptyClosureReference (EmptyClosureReference CompileTimeEnvironment -> 'ok))
(define (analyze-EmptyClosureReference exp cenv)
  'ok)

(: analyze-Seq (Seq CompileTimeEnvironment -> 'ok))
(define (analyze-Seq exp cenv)
  (match exp
	 [(struct Seq (actions))
	  (for-each (lambda: ([e : Expression])
			     (analyze e cenv))
		    actions)
	  'ok]))

(: analyze-Splice (Splice CompileTimeEnvironment -> 'ok))
(define (analyze-Splice exp cenv)
  (match exp
	 [(struct Splice (actions))
	  (for-each (lambda: ([e : Expression])
			     (analyze e cenv))
		    actions)
	  'ok]))

(: analyze-Begin0 (Begin0 CompileTimeEnvironment -> 'ok))
(define (analyze-Begin0 exp cenv)
  (match exp
	 [(struct Begin0 (actions))
	  (for-each (lambda: ([e : Expression])
			     (analyze e cenv))
		    actions)
	  'ok]))

(: analyze-App (App CompileTimeEnvironment -> 'ok))
(define (analyze-App exp cenv)
  (match exp
	 [(struct App (operator operands))
	  (let ([extended-cenv (extend/unknowns cenv (length operands))])
	    (analyze operator extended-cenv)
	    (for-each (lambda: ([o : Expression])
			       (analyze o extended-cenv))
		      operands)
	    'ok)]))

(: analyze-Let1 (Let1 CompileTimeEnvironment -> 'ok))
(define (analyze-Let1 exp cenv)
  (match exp
	 [(struct Let1 (rhs body))
	  (analyze rhs 
		   (extend/unknowns cenv 1))
	  (analyze body
		   (cons (extract-static-knowledge 
			  rhs
			  (extend/unknowns cenv 1))
			 cenv))]))

(: analyze-LetVoid (LetVoid CompileTimeEnvironment -> 'ok))
(define (analyze-LetVoid exp cenv)
  (match exp 
	 [(struct LetVoid (count body boxes?))
	  (analyze body (extend/unknowns cenv count))]))


(: analyze-InstallValue (InstallValue CompileTimeEnvironment -> 'ok))
(define (analyze-InstallValue exp cenv)
  (match exp
	 [(struct InstallValue (count depth body box?))
	  (analyze body cenv)]))


(: analyze-BoxEnv (BoxEnv CompileTimeEnvironment -> 'ok))
(define (analyze-BoxEnv exp cenv)
  (match exp
	 [(struct BoxEnv (depth body))
	  (analyze body cenv)]))


(: analyze-LetRec (LetRec CompileTimeEnvironment -> 'ok))
(define (analyze-LetRec exp cenv)
  (match exp
	 [(struct LetRec (procs body))
	  (let* ([n (length procs)]
		 [extended-cenv
		  (append (map (lambda: ([p : Expression])
				 (extract-static-knowledge p cenv))
			       procs)
			  (drop cenv n))])
	    (for-each (lambda: ([p : Expression])
			       (analyze p extended-cenv))
		      procs)
	    (analyze body extended-cenv))]))


(: analyze-WithContMark (WithContMark CompileTimeEnvironment -> 'ok))
(define (analyze-WithContMark exp cenv)
  (match exp
	 [(struct WithContMark (key value body))
	  (analyze key cenv)
	  (analyze value cenv)
	  (analyze body cenv)]))

(: analyze-ApplyValues (ApplyValues CompileTimeEnvironment -> 'ok))
(define (analyze-ApplyValues exp cenv)
  (match exp
	 [(struct ApplyValues (proc args-expr))
	  (analyze args-expr cenv)
	  (analyze proc cenv)]))


(: analyze-DefValues (DefValues CompileTimeEnvironment -> 'ok))
(define (analyze-DefValues exp cenv)
  (match exp
	 [(struct DefValues (ids rhs))
	  (analyze rhs cenv)]))


(: analyze-PrimitiveKernelValue (PrimitiveKernelValue CompileTimeEnvironment -> 'ok))
(define (analyze-PrimitiveKernelValue exp cenv)
  'ok)

(: analyze-VariableReference (VariableReference CompileTimeEnvironment -> 'ok))
(define (analyze-VariableReference exp cenv)
  'ok)

(: analyze-Require (Require CompileTimeEnvironment -> 'ok))
(define (analyze-Require exp cenv)
  'ok)




(: annotate (Expression CompileTimeEnvironmentEntry -> 'ok))
;; Accumulate information about an expression into the map.
(define (annotate exp info)
  (let ([my-map (current-expression-map)])
    (hash-set! my-map exp info)
    'ok))




(: extend/unknowns
   (CompileTimeEnvironment Natural -> CompileTimeEnvironment))
(define (extend/unknowns cenv n)
  (append (build-list n (lambda: ([i : Natural])
                                 '?))
          cenv))




(: extract-static-knowledge (Expression CompileTimeEnvironment ->  
                                        CompileTimeEnvironmentEntry))
;; Statically determines what we know about the expression, given the compile time environment.
;; We should do more here eventually, including things like type inference or flow analysis, so that
;; we can generate better code.
(define (extract-static-knowledge exp cenv)
  (cond
   [(Lam? exp)
    (make-StaticallyKnownLam (Lam-name exp)
			     (Lam-entry-label exp)
			     (if (Lam-rest? exp)
				 (make-ArityAtLeast (Lam-num-parameters exp))
				 (Lam-num-parameters exp)))]

   [(and (LocalRef? exp) (not (LocalRef-unbox? exp)))
    (let ([entry (list-ref cenv (LocalRef-depth exp))])
      entry)]
   
   [(ToplevelRef? exp)
    (let: ([name : (U Symbol False GlobalBucket ModuleVariable)
		 (list-ref (Prefix-names 
			    (ensure-prefix 
			     (list-ref cenv (ToplevelRef-depth exp))))
			   (ToplevelRef-pos exp))])
	  (cond
	   [(ModuleVariable? name)
	    name]
	   [(GlobalBucket? name)
	    '?]
	   [else
	    '?]))]
   
   [(Constant? exp)
    (make-Const (Constant-v exp))]
   
   [else
    '?]))




(: analysis-lookup (Analysis Expression -> CompileTimeEnvironmentEntry))
(define (analysis-lookup an-analysis an-exp)
  (cond
    [(Lam? exp)
     (make-StaticallyKnownLam (Lam-name exp)
                              (Lam-entry-label exp)
                              (if (Lam-rest? exp)
                                  (make-ArityAtLeast (Lam-num-parameters exp))
                                  (Lam-num-parameters exp)))]
    
    [(and (LocalRef? exp) (not (LocalRef-unbox? exp)))
     (hash-ref (Analysis-ht an-analysis) an-exp '?)]

    
    [(ToplevelRef? exp)
     (hash-ref (Analysis-ht an-analysis) an-exp '?)]
    
    [(Constant? exp)
     (make-Const (Constant-v exp))]
    
    [else
     '?]))


(: analysis-alias! (Analysis Expression Expression -> Void))
(define (analysis-alias! an-analysis from to)
  (hash-set! (Analysis-ht an-analysis) to
            (analysis-lookup an-analysis from)))



(: ensure-prefix (Any -> Prefix))
(define (ensure-prefix x)
  (if (Prefix? x)
      x
      (error 'ensure-prefix "Not a prefix: ~e" x)))