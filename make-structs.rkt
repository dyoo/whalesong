#lang typed/racket/base

(require "compiler.rkt"
         "il-structs.rkt"
         "get-dependencies.rkt"
         "lexical-structs.rkt"
         "bootstrapped-primitives.rkt"
         "compiler-structs.rkt"
         "expression-structs.rkt"
         "parameters.rkt"
         "sets.rkt"
         racket/list
         racket/match)



(provide (all-defined-out))


(define-type Source (U StatementsSource
                       MainModuleSource
                       ModuleSource
                       SexpSource))

(define-struct: StatementsSource ([stmts : (Listof Statement)]))
(define-struct: MainModuleSource ([source : Source]))
(define-struct: ModuleSource ([path : Path]))
(define-struct: SexpSource ([sexp : Any]))


(define-struct: Configuration
  ([should-follow? : (Path -> Boolean)]
   [on-module-statements : ((U Expression #f)
                            (Listof Statement)
                            -> Void)]
   [after-module-statements : ((U Expression #f)
                               (Listof Statement)
                               -> Void)]
   [after-last : (-> Void)])
  #:mutable)

(define debug-configuration
  (make-Configuration
   (lambda (p) #t)
   (lambda (ast stmt)
     (when (and ast (expression-module-path ast))
       (printf "debug build configuration: visiting ~s\n"
               (expression-module-path ast))))
   (lambda (ast stmt)
     (void))
   (lambda ()
     (void))))





(: only-bootstrapped-code : StatementsSource)
(define only-bootstrapped-code
  (make-StatementsSource (get-bootstrapping-code)))


