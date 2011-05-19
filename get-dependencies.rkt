#lang typed/racket/base
(require "expression-structs.rkt"
	 "lexical-structs.rkt"
	 "sets.rkt"
	 racket/match)

;; Collect the complete list of dependencies for a module.


(provide get-dependencies get-dependencies*)




(: get-dependencies (Expression -> (Listof ModuleName)))
(define (get-dependencies expr)
  (let ([deps ((inst new-set ModuleName))])
    (let: visit : 'ok ([expr : Expression expr])
          (cond
                 [(Top? expr)
                  (visit (Top-code expr))
                  'ok]
                 [(Constant? expr)
                  'ok]
                 [(ToplevelRef? expr)
                  'ok]
                 [(ToplevelSet? expr)
                  'ok]
                 [(LocalRef? expr)
                  'ok]
                 [(Branch? expr)
                  (visit (Branch-predicate expr))
                  (visit (Branch-consequent expr))
                  (visit (Branch-alternative expr))
                  'ok]
                 [(Lam? expr)
                  (visit (Lam-body expr))
                  'ok]
                 [(CaseLam? expr)
                  (for-each visit (CaseLam-clauses expr))
                  'ok]
                 [(EmptyClosureReference? expr)
                  'ok]
                 [(Seq? expr)
                  (for-each visit (Seq-actions expr))
                  'ok]
                 [(Splice? expr)
                  (for-each visit (Splice-actions expr))
                  'ok]
                 [(Begin0? expr)
                  (for-each visit (Begin0-actions expr))
                  'ok]
                 [(App? expr)
                  (visit (App-operator expr))
                  (for-each visit (App-operands expr))
                  'ok]
                 [(Let1? expr)
                  (visit (Let1-rhs expr))
                  (visit (Let1-body expr))
                  'ok]
                 [(LetVoid? expr)
                  (visit (LetVoid-body expr))
                  'ok]
                 [(LetRec? expr)
                  (for-each visit (LetRec-procs expr))
                  (visit (LetRec-body expr))
                  'ok]
                 [(InstallValue? expr)
                  (visit (InstallValue-body expr))
                  'ok]
                 [(BoxEnv? expr)
                  (visit (BoxEnv-body expr))
                  'ok]
                 [(WithContMark? expr)
                  (visit (WithContMark-key expr))
                  (visit (WithContMark-value expr))
                  (visit (WithContMark-body expr))
                  'ok]
                 [(ApplyValues? expr)
                  (visit (ApplyValues-proc expr))
                  (visit (ApplyValues-args-expr expr))
                  'ok]
                 [(DefValues? expr)
                  (visit (DefValues-rhs expr))
                  'ok]
                 [(PrimitiveKernelValue? expr)
                  'ok]
                 [(Module? expr)
                  (for-each (lambda: ([mn : ModuleName])
                                     (set-insert! deps mn))
                            (Module-requires expr))
                  'ok]
                 [(VariableReference? expr)
                  'ok]
                 [(Require? expr)
                  (set-insert! deps (Require-path expr))
                  'ok]))
    (set->list deps)))




(: get-dependencies* (Expression -> (Listof ModuleName)))
(define (get-dependencies* expr)
  '())