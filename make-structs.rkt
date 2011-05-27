#lang typed/racket/base

(require "compiler/compiler.rkt"
         "compiler/il-structs.rkt"
         "compiler/lexical-structs.rkt"
         "compiler/bootstrapped-primitives.rkt"
         "compiler/compiler-structs.rkt"
         "compiler/expression-structs.rkt"

         "get-dependencies.rkt"
         "parameters.rkt"
         "sets.rkt"
         racket/list
         racket/match)



(provide (all-defined-out))


(define-type Source (U StatementsSource
                       MainModuleSource
                       ModuleSource
                       SexpSource))

(define-struct: StatementsSource ([stmts : (Listof Statement)])
  #:transparent)
(define-struct: MainModuleSource ([source : Source])
  #:transparent)
(define-struct: ModuleSource ([path : Path])
  #:transparent)
(define-struct: SexpSource ([sexp : Any])
  #:transparent)


(define-struct: Configuration
  ([should-follow? : (Source Path -> Boolean)]
   [on-module-statements : (Source
                            (U Expression #f)
                            (Listof Statement)
                            -> Void)]
   [after-module-statements : (Source
                               (U Expression #f)
                               (Listof Statement)
                               -> Void)]
   [after-last : (-> Void)])
  #:mutable)

(define debug-configuration
  (make-Configuration
   (lambda (src p) #t)
   (lambda (src ast stmt)
     (when (and ast (expression-module-path ast))
       (printf "debug build configuration: visiting ~s\n"
               (expression-module-path ast))))
   (lambda (src ast stmt)
     (void))
   (lambda ()
     (void))))





(: only-bootstrapped-code : StatementsSource)
(define only-bootstrapped-code
  (make-StatementsSource (get-bootstrapping-code)))


