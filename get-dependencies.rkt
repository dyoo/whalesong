#lang typed/racket/base
(require "expression-structs.rkt"
	 "lexical-structs.rkt"
	 "sets.rkt"
	 racket/match)

;; Collect the complete list of dependencies for a module.


(provide get-dependencies)


(: get-dependencies (Expression -> (Listof ModuleName)))
(define (get-dependencies expr)
  (let ([deps ((inst new-set ModuleName))])
    (let: visit : 'ok ([expr : Expression expr])
          (cond
                 [(Top? expr)
                  (visit (Top-code expr))
                  'ok]
                 [(Module? expr)
                  (for-each (lambda: ([mn : ModuleName])
                                     (set-insert! deps mn))
                            (Module-requires expr))
                  'ok]
                 [else
                  'ok]))
    (set->list deps)))
