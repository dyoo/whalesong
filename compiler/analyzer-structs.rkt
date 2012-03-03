#lang typed/racket/base


(require "arity-structs.rkt"
         "expression-structs.rkt"
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
  (U '?                     ;; no knowledge
     AugmentedPrefix        ;; necessary since the toplevel lives in the environment too
     StaticallyKnownLam     ;; The value is a known lam
     ModuleVariable         ;; The value is a variable from a module
     PrimitiveKernelValue
     Const
     ))

(define-type PrefixElement (U False Symbol GlobalBucket ModuleVariable))
(define-predicate PrefixElement? PrefixElement)
(define-struct: AugmentedPrefixElement ([val : PrefixElement]
                                        [static : (U StaticallyKnownLam PrimitiveKernelValue Const)]))


;; This prefix is intended to know a bit more about what values are stored in the toplevel.
(define-struct: AugmentedPrefix ([names : (Listof (U PrefixElement
                                                     AugmentedPrefixElement))])
  #:transparent)




(define-struct: StaticallyKnownLam ([name : (U Symbol LamPositionalName)]
                                    [entry-point : Symbol]
                                    [arity : Arity]) #:transparent)





(define-struct: Analysis ([ht : (HashTable Expression CompileTimeEnvironmentEntry)]))


(: empty-analysis (-> Analysis))
(define (empty-analysis)
  (make-Analysis (make-hash)))