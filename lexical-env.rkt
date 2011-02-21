#lang typed/racket/base

(require racket/list
         "typed-structs.rkt")
(provide find-variable extend-lexical-environment)


;; find-variable: symbol compile-time-environment -> lexical-address
;; Find where the variable should be located.
(: find-variable (Symbol CompileTimeEnvironment -> LexicalAddress))
(define (find-variable name cenv)
  (: find-pos (Symbol (Listof Symbol) -> Natural))
  (define (find-pos sym los)
    (cond
      [(eq? sym (car los))
       0]
      [else
       (add1 (find-pos sym (cdr los)))]))
  (let: loop : LexicalAddress ([cenv : CompileTimeEnvironment cenv]
                               [depth : Natural 0])
        (cond [(empty? cenv)
               'not-found]
              [(member name (first cenv))
               (list depth (find-pos name (first cenv)))]
              [else
               (loop (rest cenv) (add1 depth))])))



;; extend-lexical-environment: lexical-environment (listof symbol) -> lexical-envrionment
(: extend-lexical-environment (CompileTimeEnvironment (Listof Symbol) -> CompileTimeEnvironment))
(define (extend-lexical-environment cenv names)
  (cons names cenv))