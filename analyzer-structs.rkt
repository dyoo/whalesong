#lang typed/racket/base


(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "kernel-primitives.rkt"
	 "il-structs.rkt")


(provide (all-defined-out))


;; Static knowledge about an expression.
;;
;; We try to keep at compile time a mapping from environment positions to
;; statically known things, to generate better code.


(define-type CompileTimeEnvironment (Listof CompileTimeEnvironmentEntry))

(define-type CompileTimeEnvironmentEntry 
  (U '?          ;; no knowledge
     Prefix      ;; placeholder: necessary since the toplevel lives in the environment too
     StaticallyKnownLam     ;; The value is a known lam
     ModuleVariable         ;; The value is a known module variable
     Const
     ))


(define-struct: StaticallyKnownLam ([name : (U Symbol LamPositionalName)]
                                    [entry-point : Symbol]
                                    [arity : Arity]) #:transparent)





(define-struct: Analysis ([ht : (HashTable Expression CompileTimeEnvironmentEntry)]))


(: empty-analysis (-> Analysis))
(define (empty-analysis)
  (make-Analysis (make-hash)))