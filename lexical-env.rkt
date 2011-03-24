#lang typed/racket/base

(require racket/list
         "lexical-structs.rkt"
         "sets.rkt")
(provide find-variable 
         extend-lexical-environment
         extend-lexical-environment/names
         extend-lexical-environment/parameter-names
         extend-lexical-environment/boxed-names
         extend-lexical-environment/placeholders
         
         collect-lexical-references
         lexical-references->compile-time-environment
         place-prefix-mask
         adjust-env-reference-depth
         env-reference-depth)


;; Find where the variable is located in the lexical environment
(: find-variable (Symbol ParseTimeEnvironment -> LexicalAddress))
(define (find-variable name cenv)
  (: find-pos (Symbol (Listof (U Symbol False)) -> Natural))
  (define (find-pos sym los)
    (cond
      [(eq? sym (car los))
       0]
      [else
       (add1 (find-pos sym (cdr los)))]))
  (let: loop : LexicalAddress
        ([cenv : ParseTimeEnvironment cenv]
         [depth : Natural 0])
        (cond [(empty? cenv)
               (error 'find-variable "~s not in lexical environment" name)]
              [else
               (let: ([elt : ParseTimeEnvironmentEntry (first cenv)])
                 (cond
                   [(Prefix? elt)
                    (cond [(member name (Prefix-names elt))
                           (make-EnvPrefixReference depth 
                                                    (find-pos name (Prefix-names elt)))]
                          [else
                           (loop (rest cenv) (add1 depth))])]
                   
                   [(NamedBinding? elt)
                    (cond
                      [(eq? (NamedBinding-name elt) name)
                       (make-EnvLexicalReference depth (NamedBinding-boxed? elt))]
                      [else
                       (loop (rest cenv) (add1 depth))])]
                   
                   [(eq? elt #f)
                    (loop (rest cenv) (add1 depth))]))])))


(: list-index (All (A) A (Listof A) -> (U #f Natural)))
(define (list-index x l)
  (let: loop : (U #f Natural) ([i : Natural 0]
                               [l : (Listof A) l])
        (cond
          [(empty? l)
           #f]
          [(eq? x (first l))
           i]
          [else
           (loop (add1 i) (rest l))])))


(: extend-lexical-environment 
   (ParseTimeEnvironment ParseTimeEnvironmentEntry -> ParseTimeEnvironment))
;; Extends the lexical environment with procedure bindings.
(define (extend-lexical-environment cenv extension)
  (cons extension cenv))



(: extend-lexical-environment/names (ParseTimeEnvironment (Listof Symbol) (Listof Boolean) ->
                                                            ParseTimeEnvironment))
(define (extend-lexical-environment/names cenv names boxed?)
  (append (map (lambda: ([n : Symbol]
                         [b : Boolean]) (make-NamedBinding n #f b)) names boxed?)
          cenv))

(: extend-lexical-environment/parameter-names (ParseTimeEnvironment (Listof Symbol) (Listof Boolean) -> ParseTimeEnvironment))
(define (extend-lexical-environment/parameter-names cenv names boxed?)
  (append (map (lambda: ([n : Symbol]
                         [b : Boolean]) 
                        (make-NamedBinding n #t b)) names boxed?) 
          cenv))

(: extend-lexical-environment/boxed-names (ParseTimeEnvironment (Listof Symbol) -> ParseTimeEnvironment))
(define (extend-lexical-environment/boxed-names cenv names)
  (append (map (lambda: ([n : Symbol]) (make-NamedBinding n #f #t)) names)
          cenv))


(: extend-lexical-environment/placeholders
   (ParseTimeEnvironment Natural -> ParseTimeEnvironment))
;; Add placeholders to the lexical environment (This represents what happens during procedure application.)
(define (extend-lexical-environment/placeholders cenv n)
  (append (build-list n (lambda: ([i : Natural]) #f))
          cenv))
  



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
                     [(EnvLexicalReference? addr)
                      (set-insert! lexical-references
                                   addr)
                      (loop (rest addresses))]
                     [(EnvPrefixReference? addr)
                      (set-insert! prefix-references
                                   (make-EnvWholePrefixReference (EnvPrefixReference-depth addr)))
                      (loop (rest addresses))]))]))))



(: lexical-references->compile-time-environment ((Listof EnvReference) ParseTimeEnvironment ParseTimeEnvironment
                                                                     (Listof Symbol)
                                                                       -> ParseTimeEnvironment))
;; Creates a lexical environment containing the closure's bindings.
(define (lexical-references->compile-time-environment refs cenv new-cenv symbols-to-keep)
  (let: loop : ParseTimeEnvironment ([refs : (Listof EnvReference) (reverse refs)]
                                       [new-cenv : ParseTimeEnvironment new-cenv])
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
                          (cons (place-prefix-mask 
                                 (ensure-Prefix (list-ref cenv (EnvWholePrefixReference-depth a-ref)))
                                 symbols-to-keep)
                                new-cenv))]))])))

(: ensure-Prefix (Any -> Prefix))
(define (ensure-Prefix x)
  (if (Prefix? x)
      x
      (error 'ensure-Prefix "~s" x)))



(: place-prefix-mask (Prefix (Listof Symbol) -> Prefix))
;; Masks elements of the prefix off.
(define (place-prefix-mask a-prefix symbols-to-keep)
  (make-Prefix
   (map (lambda: ([n : (U Symbol False)])
                 (cond [(symbol? n)
                        (if (member n symbols-to-keep)
                            n
                            #f)]
                       [else n]))
        (Prefix-names a-prefix))))



(: adjust-env-reference-depth (EnvReference Natural -> EnvReference))
(define (adjust-env-reference-depth target n)
  (cond
    [(EnvLexicalReference? target)
     (make-EnvLexicalReference (+ n (EnvLexicalReference-depth target))
                               (EnvLexicalReference-unbox? target))]
    [(EnvPrefixReference? target)
     (make-EnvPrefixReference (+ n (EnvPrefixReference-depth target))
                              (EnvPrefixReference-pos target))]
    [(EnvWholePrefixReference? target)
     (make-EnvWholePrefixReference (+ n (EnvWholePrefixReference-depth target)))]))


(: env-reference-depth ((U EnvLexicalReference EnvPrefixReference EnvWholePrefixReference) -> Natural))
(define (env-reference-depth a-ref)
  (cond
    [(EnvLexicalReference? a-ref)
     (EnvLexicalReference-depth a-ref)]
    [(EnvPrefixReference? a-ref)
     (EnvPrefixReference-depth a-ref)]
    [(EnvWholePrefixReference? a-ref)
     (EnvWholePrefixReference-depth a-ref)]))