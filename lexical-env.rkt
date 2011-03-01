#lang typed/racket/base

(require racket/list
         "il-structs.rkt"
         "lexical-structs.rkt"
         "sets.rkt")
(provide find-variable 
         extend-lexical-environment
         extend-lexical-environment/placeholders
         lexical-environment-pop-depth
         collect-lexical-references
         lexical-references->compile-time-environment)


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
               (error 'find-variable "Unable to find ~s in the environment" name)]
              [else
               (let: ([elt : CompileTimeEnvironmentEntry (first cenv)])
                 (cond
                   [(eq? #f elt)
                    (loop (rest cenv) (add1 depth))]
                   [(Prefix? elt)
                    (cond [(member name (Prefix-names elt))
                           (make-PrefixAddress depth (find-pos name (Prefix-names elt)) name)]
                          [else
                           (loop (rest cenv) (add1 depth))])]
                   [(symbol? elt)
                    (cond
                      [(eq? name elt)
                       (make-LocalAddress depth)]
                      [else
                       (loop (rest cenv) (add1 depth))])]))])))



(: extend-lexical-environment (CompileTimeEnvironment (U (Listof Symbol) Prefix) -> CompileTimeEnvironment))
;; Extends the lexical environment with procedure bindings.
(define (extend-lexical-environment cenv names)
  (cond [(Prefix? names)
         (cons names cenv)]
        [(list? names)
         (append names cenv)]))


(: extend-lexical-environment/placeholders (CompileTimeEnvironment Natural -> CompileTimeEnvironment))
;; Add placeholders to the lexical environment (This represents what happens during procedure application.)
(define (extend-lexical-environment/placeholders cenv n)
  (cond [(= n 0)
         cenv]
        [else
         (extend-lexical-environment/placeholders (cons #f cenv) (sub1 n))]))
  

(: lexical-environment-pop-depth (CompileTimeEnvironment -> Natural))
;; Computes how many environments we need to pop till we clear the procedure arguments.
(define (lexical-environment-pop-depth cenv)
  (length cenv))



(: collect-lexical-references ((Listof LexicalAddress) 
                               -> 
                               (Listof (U EnvLexicalReference EnvWholePrefixReference))))
;; Given a list of lexical addresses, computes a set of unique references.
;; Multiple lexical addresses to a single prefix should be treated identically.
(define (collect-lexical-references addresses)
  (let: ([prefix-references : (Setof EnvWholePrefixReference) (new-set)]
         [lexical-references : (Setof EnvLexicalReference) (new-set)])
        (let: loop : (Listof (U EnvLexicalReference EnvWholePrefixReference)) 
              ([addresses : (Listof LexicalAddress) addresses])
              (cond 
                [(empty? addresses)
                 (append (set->list prefix-references) (set->list lexical-references))]
                [else
                 (let ([addr (first addresses)])
                   (cond
                     [(LocalAddress? addr)
                      (set-insert! lexical-references
                                   (make-EnvLexicalReference (LocalAddress-depth addr)))
                      (loop (rest addresses))]
                     [(PrefixAddress? addr)
                      (set-insert! prefix-references
                                   (make-EnvWholePrefixReference (PrefixAddress-depth addr)))
                      (loop (rest addresses))]))]))))


(define-type EnvReference (U EnvLexicalReference EnvWholePrefixReference))

(: lexical-references->compile-time-environment ((Listof EnvReference) CompileTimeEnvironment CompileTimeEnvironment
                                                                       -> CompileTimeEnvironment))
;; Creates a lexical environment containing the closure's bindings.
(define (lexical-references->compile-time-environment refs cenv new-cenv)
  (let: loop : CompileTimeEnvironment ([refs : (Listof EnvReference) (reverse refs)]
                                       [new-cenv : CompileTimeEnvironment new-cenv])
        (cond
          [(empty? refs)
           new-cenv]
          [else
           (let: ([a-ref : EnvReference (first refs)])
                 (cond
                   [(EnvLexicalReference? a-ref)
                    (loop (rest refs)
                          (cons (list-ref cenv (EnvLexicalReference-depth a-ref))
                                new-cenv))]
                   [(EnvWholePrefixReference? a-ref)
                    (loop (rest refs)
                          (cons (list-ref cenv (EnvWholePrefixReference-depth a-ref))
                                new-cenv))]))])))
