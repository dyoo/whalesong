#lang typed/racket/base
(require "compiler/expression-structs.rkt"
	 "compiler/lexical-structs.rkt"
	 "sets.rkt"
	 racket/match)

;; Collect the complete list of dependencies for a module.


(provide get-dependencies
         expression-module-path)


(: get-dependencies (Expression -> (Listof ModuleLocator)))
(define (get-dependencies expr)
  (let ([deps ((inst new-set ModuleLocator))])
    (let: visit : 'ok ([expr : Expression expr])
          (cond
                 [(Top? expr)
                  (visit (Top-code expr))
                  'ok]
                 [(Module? expr)
                  (for-each (lambda: ([mn : ModuleLocator])
                                     (set-insert! deps mn))
                            (Module-requires expr))
                  'ok]
                 [else
                  'ok]))
    (set->list deps)))


(: expression-module-path (Expression -> (U False ModuleLocator)))
;; Given a toplevel expression of a module, returns its self-declared ModuleLocator.
;; If we can't find one, return false.
(define (expression-module-path expr)
  (cond
   [(Top? expr)
    (expression-module-path (Top-code expr))]
   [(Module? expr)
    (Module-path expr)]
   [else
    #f]))