#lang typed/racket/base

(require "../compiler/il-structs.rkt"
         "../compiler/bootstrapped-primitives.rkt"
         "../compiler/expression-structs.rkt"
         "get-dependencies.rkt")



(provide (all-defined-out))


(define-type Source (U StatementsSource
                       MainModuleSource
                       ModuleSource
                       SexpSource
                       UninterpretedSource
                       ))

(define-struct: StatementsSource ([stmts : (Listof Statement)])
  #:transparent)
(define-struct: MainModuleSource ([source : Source])
  #:transparent)
(define-struct: ModuleSource ([path : Path])
  #:transparent)
(define-struct: SexpSource ([sexp : Any])
  #:transparent)
(define-struct: UninterpretedSource ([datum : String])
  #:transparent)



(define-struct: Configuration
  ([wrap-source : (Source -> Source)]
   [should-follow-children? : (Source -> Boolean)]
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
   (lambda (src) src)
   (lambda (src) #t)
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


