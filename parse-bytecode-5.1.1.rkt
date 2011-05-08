#lang racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt")


;; Parsing Racket 5.1.1 bytecode structures into our own.
(require compiler/zo-parse
         racket/match
         racket/list)

(provide parse-bytecode
         current-module-path-index-resolver)


;; The module-path-index of self is:
(define self-idx (module-path-index-join #f #f))

(define (self-idx? mpi)
  (let-values ([(path subpath)
                (module-path-index-split mpi)])
    (eq? path #f)))


;; current-module-path-index-resolver: (module-path-index -> ModuleName) -> void
;; The module path index resolver figures out how to translate module path indices to module names.
(define current-module-path-index-resolver 
  (make-parameter 
   (lambda (an-mpi)
     (error 'current-module-path-index-resolver))))


;; seen-lambdas: 
(define seen-lambdas (make-parameter (make-hasheq)))



;; parse-bytecode: Input-Port -> Expression
(define (parse-bytecode in)
  (parameterize ([seen-lambdas (make-hasheq)])
    (let ([compilation-top (zo-parse in)])
      (parse-top compilation-top))))


(define (parse-top a-top)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (make-Top (parse-prefix prefix) 
               (parse-top-code code))]))


(define (parse-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-Prefix 
      (append (map parse-prefix-toplevel toplevels)
              (if (empty? stxs)
                  empty
                  (list #f))
              (build-list num-lifts
                          (lambda (i)
                            #f))))]))


;; parse-top-code: (U form Any -> Expression)
(define (parse-top-code code)
  (cond
    [(form? code)
     (parse-form code)]
    [else
     (make-Constant code)]))


;; parse-prefix-toplevel: (U #f symbol global-bucket module-variable) -> (U False Symbol GlobalBucket ModuleVariable)
(define (parse-prefix-toplevel a-toplevel)
  (cond
    [(eq? a-toplevel #f)
     #f]
    [(symbol? a-toplevel)
     a-toplevel]
    [(global-bucket? a-toplevel)
     (make-GlobalBucket (global-bucket-name a-toplevel))]
    [(module-variable? a-toplevel)
     (let ([resolver (current-module-path-index-resolver)])
       (make-ModuleVariable (module-variable-sym a-toplevel)
                            (resolver self-idx (module-variable-modidx a-toplevel))))]))


;; parse-form: form -> (U Expression)
(define (parse-form a-form)
  (cond
    [(def-values? a-form)
     (parse-def-values a-form)]

    [(def-syntaxes? a-form)
     (parse-def-syntaxes a-form)]
    
    [(req? a-form)
     (parse-req a-form)]
    
    [(seq? a-form)
     (parse-seq a-form)]
    
    [(splice? a-form)
     (parse-splice a-form)]
    
    [(mod? a-form)
     (parse-mod a-form)]
    
    [(expr? a-form)
     (parse-expr a-form)]

    [else
     (error 'parse-form "~s" a-form)]))


;; parse-def-values: def-values -> Expression
(define (parse-def-values form)
  (match form 
    [(struct def-values (ids rhs))
     (make-DefValues (map parse-toplevel ids)
                     (parse-def-values-body rhs))]))

;; parse-def-values-body: (U expr seq Any) -> Expression
(define (parse-def-values-body rhs)
  (cond
    [(expr? rhs)
     (parse-expr rhs)]
    [(seq? rhs)
     (parse-seq rhs)]
    [else
     (make-Constant rhs)]))



(define (parse-def-syntaxes form)
  ;; Currently, treat def-syntaxes as a no-op.  The compiler will not produce
  ;; syntax transformers.
  (make-Constant (void)))


(define (parse-req form)
  (error 'fixme))


;; parse-seq: seq -> Expression
(define (parse-seq form)
  (match form
    [(struct seq (forms))
     (make-Seq (map parse-form-item forms))]))

;; parse-form-item: (U form Any) -> Expression
(define (parse-form-item item)
  (cond
    [(form? item)
     (parse-form item)]
    [else
     (make-Constant item)]))


;; parse-splice: splice -> Expression
(define (parse-splice form)
  (match form
    [(struct splice (forms))
     (make-Splice (map parse-splice-item forms))]))


;; parse-splice-item: (U form Any) -> Expression
(define (parse-splice-item item)
  (cond
    [(form? item)
     (parse-form item)]
    [else
     (make-Constant item)]))


;; parse-mod: mod -> Expression
(define (parse-mod form)
  (match form
    [(struct mod (name srcname self-modidx prefix provides requires
       body syntax-body unexported max-let-depth dummy lang-info
       internal-context))
     (let ([resolver (current-module-path-index-resolver)])
       (make-Module (make-ModuleName name)
                    (parse-prefix prefix)
                    (parse-mod-requires self-modidx requires)
                    (parse-mod-provides provides)
                    (parse-mod-body body)))]))


;; parse-mod-requires: module-path-index (listof (pair (U Integer #f) (listof module-path-index))) -> (listof ModuleName)
(define (parse-mod-requires enclosing-module-path-index requires)
  ;; We only care about phase 0 --- the runtime.
  (let ([resolver (current-module-path-index-resolver)])
    (let loop ([requires requires])
      (cond
        [(empty? requires)
         empty]
        [(= (car (first requires))
              0)
         (map (lambda (m) (resolver enclosing-module-path-index m))
              (cdr (first requires)))]
        [else
         (loop (rest requires))]))))


(define (parse-mod-provides provides)
  (let* ([resolver (current-module-path-index-resolver)]
         [parse-provided (lambda (a-provided)
                           (match a-provided
                             [(struct provided (name src src-name nom-mod src-phase protected? insp))
                              ;; fixme: we're not considering all of the fields here...
                              (make-Provided name src-name)]))])
    (let loop ([provides provides])
      (cond
        [(empty? provides)
         empty]
        [(= (first (first provides)) 0)
         (parse-provided (second (first provides)))]
        [else
         (loop (rest provides))]))))




;; parse-mod-body: (listof (or/c form? any/c))
(define (parse-mod-body body)
  (let ([parse-item (lambda (item)
                      (cond
                        [(form? item)
                         (parse-form item)]
                        [else
                         (make-Constant item)]))])
  (make-splice (map parse-item body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-expr expr)
  (cond
    [(lam? expr)
     (parse-lam expr)]
    [(closure? expr)
     (parse-closure expr)]
    [(case-lam? expr)
     (parse-case-lam expr)]
    [(let-one? expr)
     (parse-let-one expr)]
    [(let-void? expr)
     (parse-let-void expr)]
    [(install-value? expr)
     (parse-install-value expr)]
    [(let-rec? expr)
     (parse-let-rec expr)]
    [(boxenv? expr)
     (parse-boxenv expr)]
    [(localref? expr)
     (parse-localref expr)]
    [(toplevel? expr)
     (parse-toplevel expr)]
    [(topsyntax? expr)
     (parse-topsyntax expr)]
    [(application? expr)
     (parse-application expr)]
    [(branch? expr)
     (parse-branch expr)]
    [(with-cont-mark? expr)
     (parse-with-cont-mark expr)]
    [(beg0? expr)
     (parse-beg0 expr)]
    [(varref? expr)
     (parse-varref expr)]
    [(assign? expr)
     (parse-assign expr)]
    [(apply-values? expr)
     (parse-apply-values expr)]
    [(primval? expr)
     (parse-primval expr)]))

(define (parse-lam expr)
  (match expr
    [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     (let ([lam-name (cond
                       [(symbol? name)
                        name]
                       [(vector? name)
                        (string->symbol (format "~s" name))]
                       [else
                        (error "lam name neither symbol nor vector: ~e" name)])])
       (make-Lam lam-name 
                 num-params 
                 rest?
                 (parse-lam-body body)
                 (vector->list closure-map)
                 (make-label 'lamEntry)))]))
               
(define (parse-lam-body body)
  (cond
    [(expr? body)
     (parse-expr body)]
    [(seq? body)
     (parse-seq body)]
    [else
     (make-Constant body)]))



(define (parse-closure expr)
  (match expr
    [(struct closure (code gen-id))
     ;; Fixme: we must handle cycles here.
     (parse-lam code)]))


(define (parse-case-lam exp)
  (error 'fixme))

(define (parse-let-one expr)
  (error 'fixme))

(define (parse-let-void expr)
  (error 'fixme))

(define (parse-install-value expr)
  (error 'fixme))

(define (parse-let-rec expr)
  (error 'fixme))

(define (parse-boxenv expr)
  (error 'fixme))

(define (parse-localref expr)
  (match expr
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     ;; FIXME: we should use clear? at the very least: as I understand it,
     ;; this is here to maintain safe-for-space behavior.
     ;; We should also make use of flonum information to generate better code.
     (make-LocalRef pos unbox?)]))


(define (parse-toplevel expr)
  (match expr
    ;; FIXME: we should also keep track of const? and ready? to produce better code.
    [(struct toplevel (depth pos const? ready?))
     (make-ToplevelRef depth pos)]))

(define (parse-topsyntax expr)
  (error 'fixme))

(define (parse-application expr)
  (error 'fixme))

(define (parse-branch expr)
  (error 'fixme))

(define (parse-with-cont-mark expr)
  (error 'fixme))

(define (parse-beg0 expr)
  (error 'fixme))

(define (parse-varref expr)
  (error 'fixme))

(define (parse-assign expr)
  (error 'fixme))

(define (parse-apply-values expr)
  (error 'fixme))

(define (parse-primval expr)
  (error 'fixme))