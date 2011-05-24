#lang typed/racket/base

(provide (rename-out [-analyze analyze]))

(require "analyzer-structs.rkt"
	 "expression-structs.rkt"
	 racket/match)




(: current-expression-map
   (Parameterof (HashTable Expression CompileTimeEnvironmentEntry)))
(define current-expression-map (make-parameter 
				((inst make-hasheq Expression
				       CompileTimeEnvironmentEntry))))




(: -analyze (Expression -> (HashTable Expression CompileTimeEnvironmentEntry)))
(define (-analyze exp)
  (parameterize ([current-expression-map 
		  ((inst make-hasheq Expression CompileTimeEnvironmentEntry))])
    (analyze exp '())
    (current-expression-map)))




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
	  (analyze code (cons (Top-prefix exp) cenv))]))


(: analyze-Module (Module CompileTimeEnvironment -> 'ok))
(define (analyze-Module exp cenv)
  'ok)

(: analyze-Constant (Constant CompileTimeEnvironment -> 'ok))
(define (analyze-Constant exp cenv)
  'ok)

(: analyze-LocalRef (LocalRef CompileTimeEnvironment -> 'ok))
(define (analyze-LocalRef exp cenv)
  'ok)

(: analyze-ToplevelRef (ToplevelRef CompileTimeEnvironment -> 'ok))
(define (analyze-ToplevelRef exp cenv)
  'ok)

(: analyze-ToplevelSet (ToplevelSet CompileTimeEnvironment -> 'ok))
(define (analyze-ToplevelSet exp cenv)
  'ok)

(: analyze-Branch (Branch CompileTimeEnvironment -> 'ok))
(define (analyze-Branch exp cenv)
  'ok)

(: analyze-Lam (Lam CompileTimeEnvironment -> 'ok))
(define (analyze-Lam exp cenv)
  'ok)
(: analyze-CaseLam (CaseLam CompileTimeEnvironment -> 'ok))
(define (analyze-CaseLam exp cenv)
  'ok)

(: analyze-EmptyClosureReference (EmptyClosureReference CompileTimeEnvironment -> 'ok))
(define (analyze-EmptyClosureReference exp cenv)
  'ok)

(: analyze-Seq (Seq CompileTimeEnvironment -> 'ok))
(define (analyze-Seq exp cenv)
  'ok)

(: analyze-Splice (Splice CompileTimeEnvironment -> 'ok))
(define (analyze-Splice exp cenv)
  'ok)

(: analyze-Begin0 (Begin0 CompileTimeEnvironment -> 'ok))
(define (analyze-Begin0 exp cenv)
  'ok)

(: analyze-App (App CompileTimeEnvironment -> 'ok))
(define (analyze-App exp cenv)
  'ok)

(: analyze-Let1 (Let1 CompileTimeEnvironment -> 'ok))
(define (analyze-Let1 exp cenv)
  'ok)

(: analyze-LetVoid (LetVoid CompileTimeEnvironment -> 'ok))
(define (analyze-LetVoid exp cenv)
  'ok)

(: analyze-InstallValue (InstallValue CompileTimeEnvironment -> 'ok))
(define (analyze-InstallValue exp cenv)
  'ok)

(: analyze-BoxEnv (BoxEnv CompileTimeEnvironment -> 'ok))
(define (analyze-BoxEnv exp cenv)
  'ok)

(: analyze-LetRec (LetRec CompileTimeEnvironment -> 'ok))
(define (analyze-LetRec exp cenv)
  'ok)

(: analyze-WithContMark (WithContMark CompileTimeEnvironment -> 'ok))
(define (analyze-WithContMark exp cenv)
  'ok)

(: analyze-ApplyValues (ApplyValues CompileTimeEnvironment -> 'ok))
(define (analyze-ApplyValues exp cenv)
  'ok)

(: analyze-DefValues (DefValues CompileTimeEnvironment -> 'ok))
(define (analyze-DefValues exp cenv)
  'ok)

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


