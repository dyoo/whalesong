#lang racket/base

;; Parsing Racket 5.1.1 bytecode structures into our own structures.
(require "typed-module-path.rkt"
         "lam-entry-gensym.rkt"
         "path-rewriter.rkt"
         "../compiler/expression-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../parameters.rkt"
         "../get-module-bytecode.rkt"
         syntax/modresolve
         compiler/zo-parse
         racket/path
         racket/match
         racket/list)


(provide parse-bytecode
         reset-lam-label-counter!/unit-testing)



;; current-module-path-index-resolver: (module-path-index (U Path #f) -> (U Symbol Path)) -> void
;; The module path index resolver figures out how to translate module path indices to module names.
(define current-module-path-index-resolver 
  (make-parameter 
   (lambda (mpi relative-to)
     (cond
       [(eq? mpi #f)
        (current-module-path)]
       [(self-module-path-index? mpi)
        (current-module-path)]
       [else
        (resolve-module-path-index mpi relative-to)]))))


(define current-module-path-resolver 
  (make-parameter 
   (lambda (module-path relative-to)
     (resolve-module-path module-path relative-to))))



(define (self-module-path-index? mpi)
  (let-values ([(x y) (module-path-index-split mpi)])
    (and (eq? x #f)
         (eq? y #f))))


(define (explode-module-path-index mpi)
  (let-values ([(x y) (module-path-index-split mpi)])
    (cond
      [(module-path-index? y)
       (cons x (explode-module-path-index y))]
      [else
       (list x y)])))
   



;; seen-closures: (hashof symbol -> symbol)
;; As we're parsing, we watch for closure cycles.  On any subsequent time where
;; we see a closure cycle, we break the cycle by generating an EmptyClosureReference.
;; The map is from the gen-id to the entry-point label of the lambda.
(define seen-closures (make-parameter (make-hasheq)))






;; Code is copied-and-pasted from compiler/decompile.  Maps the primval ids to their respective
;; symbolic names.
(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (namespace-require ''#%unsafe)
             (namespace-require ''#%flfxnum)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (lambda (x) 
                                                    #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let ([v (and (cdr b)
                    (zo-parse (let ([out (open-output-bytes)])
                                (write (cdr b) out)
                                (close-output-port out)
                                (open-input-bytes (get-output-bytes out)))))])
        (let ([n (match v
                   [(struct compilation-top (_ prefix (struct primval (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))








;; parse-bytecode: (U Input-Port Path) -> Expression
;;
;; Given an input port, assumes the input is the byte representation of compiled-code.
;;
;; Given a path, assumes the path is for a module.  It gets the module bytecode, and parses
;; that.
;;
;; TODO: this may be doing too much work.  It doesn't quite feel like the right elements
;; are being manipulated here.
(define (parse-bytecode in)
  (cond
    [(input-port? in)
     (parameterize ([seen-closures (make-hasheq)])
       (let ([compilation-top (zo-parse in)])
         (parse-top compilation-top)))]
    
    [(compiled-expression? in)
     (let ([op (open-output-bytes)])
       (write in op)
       (parse-bytecode (open-input-bytes (get-output-bytes op))))]

    [(path? in)
     (let*-values ([(normal-path) (normalize-path in)]
                   [(base file-path dir?) (split-path normal-path)])
       (parameterize ([current-module-path normal-path]
                      [current-directory (cond [(path? base)
                                                base]
                                               [else
                                                (error 'parse-bytecode)])])
         (parse-bytecode
          (open-input-bytes (get-module-bytecode normal-path)))))]    
    [else
     (error 'parse-bytecode "Don't know how to parse from ~e" in)]))


         




(define (parse-top a-top)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (maybe-fix-module-name
      (make-Top (parse-prefix prefix) 
                (parse-top-code code)))]))



;; maybe-fix-module-name: expression -> expression
;; When we're compiling a module directly from memory, it doesn't have a file path.
;; We rewrite the ModuleLocator to its given name.
(define (maybe-fix-module-name exp)
  (match exp
    [(struct Top (top-prefix 
                  (struct Module ((and name (? symbol?))
                                  (struct ModuleLocator ('self 'self))
                                  module-prefix
                                  module-requires
                                  module-code))))
     (make-Top top-prefix
               (make-Module name
                            (make-ModuleLocator name name) (current-module-path)
                            module-prefix
                            module-requires
                            module-code))]
    [else
     exp]))



(define (parse-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-Prefix 
      (append (map parse-prefix-toplevel toplevels)
              (map (lambda (x) #f) stxs)
              (if (empty? stxs) empty (list #f))
              (build-list num-lifts (lambda (i) #f))))]))


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
                            (let ([resolved-path-name
                                   (resolver (module-variable-modidx a-toplevel) (current-module-path))])
                              (wrap-module-name resolved-path-name))))]))

(define (wrap-module-name resolved-path-name)
  (cond
    [(symbol? resolved-path-name)
     (make-ModuleLocator resolved-path-name resolved-path-name)]
    [(path? resolved-path-name)
     (let ([rewritten-path (rewrite-path resolved-path-name)])
       (cond
         [(symbol? rewritten-path)
          (make-ModuleLocator (rewrite-path resolved-path-name)
                           (normalize-path resolved-path-name))]
         [else
          (error 'wrap-module-name "Unable to resolve module path ~s."
                 resolved-path-name)]))]))
          
       



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
                     (parse-expr-seq-constant rhs))]))



(define (parse-def-syntaxes form)
  ;; Currently, treat def-syntaxes as a no-op.  The compiler will not produce
  ;; syntax transformers.
  (make-Constant (void)))



(define (parse-req form)
  (let ([resolver (current-module-path-resolver)])
    (match form
      [(struct req (reqs dummy))
       (let ([require-statement (parse-req-reqs reqs)])
         (match require-statement
           [(list '#%require (and (? module-path?) path))
            (let ([resolved-path ((current-module-path-resolver) path (current-module-path))])
              (cond
                [(symbol? resolved-path)
                 (make-Require (make-ModuleLocator resolved-path resolved-path))]
                [(path? resolved-path)
                 (let ([rewritten-path (rewrite-path resolved-path)])
                   (cond
                     [(symbol? rewritten-path)
                      (make-Require (make-ModuleLocator rewritten-path
                                                     (normalize-path resolved-path)))]
                     [else
                      (printf "Internal error: I don't know how to handle the require for ~s" require-statement)
                      (error 'parse-req)]))]
                [else
                 (printf "Internal error: I don't know how to handle the require for ~s" require-statement)
                 (error 'parse-req)]))]
           [else
            (printf "Internal error: I don't know how to handle the require for ~s" require-statement)
            (error 'parse-req)]))])))
     
;; parse-req-reqs: (stx -> (listof ModuleLocator))
(define (parse-req-reqs reqs)
  (match reqs
    [(struct stx (encoded))
     (unwrap-wrapped encoded)]))

(define (unwrap-wrapped encoded)
  (cond [(wrapped? encoded)
         (match encoded 
           [(struct wrapped (datum wraps certs))
            (unwrap-wrapped datum)])]
        [(pair? encoded)
         (cons (unwrap-wrapped (car encoded))
               (unwrap-wrapped (cdr encoded)))]
        [(null? encoded)
         null]
        [else
         encoded]))




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
     (let ([self-path 
            ((current-module-path-index-resolver)
             self-modidx
             (current-module-path))])
       (cond
        [(symbol? self-path)
         (make-Module name
                      (make-ModuleLocator self-path self-path)
                      (parse-prefix prefix)
                      (parse-mod-requires self-modidx requires)
                      (parse-mod-body body))]
        [else
         (let ([rewritten-path (rewrite-path self-path)])
           (cond
             [(symbol? rewritten-path)
              (make-Module name
                           (make-ModuleLocator rewritten-path 
                                            (normalize-path self-path))
                           (parse-prefix prefix)
                           (parse-mod-requires self-modidx requires)
                           (parse-mod-body body))]
             [else
              (error 'parse-mod "Internal error: unable to resolve module path ~s" self-path)]))]))]))


;; parse-mod-requires: module-path-index (listof (pair (U Integer #f) (listof module-path-index))) -> (listof ModuleLocator)
(define (parse-mod-requires enclosing-module-path-index requires)
  ;; We only care about phase 0 --- the runtime.
  (let ([resolver (current-module-path-index-resolver)])
    (let loop ([requires requires])
      (cond
        [(empty? requires)
         empty]
        [(= (car (first requires))
              0)
         (map (lambda (m) 
                (let ([enclosing-path (resolver enclosing-module-path-index (current-module-path))])
                  (cond
                    [(symbol? enclosing-path)
                     (wrap-module-name (resolver m (current-module-path)))]
                    [(path? enclosing-path)
                     (wrap-module-name (resolver m enclosing-path))])))
              (cdr (first requires)))]
        [else
         (loop (rest requires))]))))






;; parse-mod-body: (listof (or/c form? any/c)) -> Expression
(define (parse-mod-body body)
  (let ([parse-item (lambda (item)
                      (cond
                        [(form? item)
                         (parse-form item)]
                        [else
                         (make-Constant item)]))])
  (make-Splice (map parse-item body))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-expr expr)
  (cond
    [(lam? expr)
     (parse-lam expr (make-lam-label))]
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

(define (parse-lam expr entry-point-label)
  (match expr
    [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     (let ([lam-name (extract-lam-name name)])
       (make-Lam lam-name 
                 num-params 
                 rest?
                 (parse-expr-seq-constant body)
                 (vector->list closure-map)
                 entry-point-label))]))
  

;; parse-closure: closure -> (U Lam EmptyClosureReference)
;; Either parses as a regular lambda, or if we come across the same closure twice,
;; breaks the cycle by creating an EmptyClosureReference with the pre-existing lambda
;; entry point.
(define (parse-closure expr)
  (match expr
    [(struct closure (code gen-id))
     (let ([seen (seen-closures)])
       (cond
         [(hash-has-key? seen gen-id)
          (match code
            [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
             (let ([lam-name (extract-lam-name name)])
               (make-EmptyClosureReference lam-name 
                                           num-params 
                                           rest?
                                           (hash-ref seen gen-id)))])]
         [else
          (let ([fresh-entry-point (make-lam-label)])
            (hash-set! seen gen-id fresh-entry-point)
            (parse-lam code fresh-entry-point))]))]))



;; extract-lam-name: (U Symbol Vector) -> (U Symbol LamPositionalName)
(define (extract-lam-name name)
  (cond
    [(symbol? name)
     name]
    [(vector? name)
     (match name
       [(vector (and (? symbol?) sym)
                (and (? path?) source) 
                (and (? number?) line)
                (and (? number?) column)
                (and (? number?) offset)
                (and (? number?) span)
                _)
        (let ([try-to-rewrite (rewrite-path source)])
          (make-LamPositionalName sym
                                  (if try-to-rewrite
                                      (symbol->string try-to-rewrite)
                                      (path->string source))
                                  line
                                  column 
                                  offset
                                  span))]
       [(vector (and (? symbol?) sym)
                (and (? symbol?) source) 
                (and (? number?) line)
                (and (? number?) column)
                (and (? number?) offset)
                (and (? number?) span)
                _)
        (make-LamPositionalName sym
                                (symbol->string source)
                                line
                                column 
                                offset
                                span)]
       [else
        (string->symbol (format "~s" name))])]
    [else
     'unknown
     ;; The documentation says that the name must be a symbol or vector, but I'm seeing cases
     ;; where it returns the empty list when there's no information available.
     ]))




(define (parse-case-lam exp)
  (match exp
    [(struct case-lam (name clauses))
     (let ([case-lam-label (make-lam-label)])
       (make-CaseLam (extract-lam-name name)
                     (map (lambda (l) 
                            (cond
                              [(closure? l)
                               (parse-closure l)]
                              [else
                               (parse-lam l (make-lam-label))]))
                          clauses)
                     case-lam-label))]))


(define (parse-let-one expr)
  (match expr
    [(struct let-one (rhs body flonum? unused?))
     ;; fixme: use flonum? and unused? to generate better code.
     (make-Let1 (parse-expr-seq-constant rhs)
                (parse-expr-seq-constant body))]))


;; parse-expr-seq-constant: (U expr seq Any) -> Expression
(define (parse-expr-seq-constant x)
  (cond
    [(expr? x) (parse-expr x)]
    [(seq? x) (parse-seq x)]
    [else (make-Constant x)]))


(define (parse-let-void expr)
  (match expr
    [(struct let-void (count boxes? body))
     (make-LetVoid count (parse-expr-seq-constant body) boxes?)]))


(define (parse-install-value expr)
  (match expr
    [(struct install-value (count pos boxes? rhs body))
     (make-Seq (list (make-InstallValue count pos (parse-expr-seq-constant rhs) boxes?)
                     (parse-expr-seq-constant body)))]))


(define (parse-let-rec expr)
  (match expr
    [(struct let-rec (procs body))
     (make-LetRec (map (lambda (p) (parse-lam p (make-lam-label)))
                       procs)
                  (parse-expr-seq-constant body))]))

(define (parse-boxenv expr)
  (match expr
    [(struct boxenv (pos body))
     (make-BoxEnv pos (parse-expr-seq-constant body))]))


(define (parse-localref expr)
  (match expr
    [(struct localref (unbox? pos clear? other-clears? flonum?))
     ;; FIXME: we should use clear? at the very least: as I understand it,
     ;; this is here to maintain safe-for-space behavior.
     ;; We should also make use of flonum information to generate better code.
     (make-LocalRef pos unbox?)]))


(define (parse-toplevel expr)
  (match expr
    ;; FIXME: we should also keep track of const? and ready? to produce better code, and to
    ;; do the required runtime checks when necessary (const?=#f, ready?=#f)
    [(struct toplevel (depth pos const? ready?))
     (make-ToplevelRef depth pos)]))


(define (parse-topsyntax expr)
  ;; We should not get into this because we're only parsing the runtime part of
  ;; the bytecode.  Treated as a no-op.
  (make-Constant (void)))


(define (parse-application expr)
  (match expr
    [(struct application (rator rands))
     (make-App (parse-application-rator rator)
               (map parse-application-rand rands))]))

(define (parse-application-rator rator)
  (cond
    [(expr? rator)
     (parse-expr rator)]
    [(seq? rator)
     (parse-seq rator)]
    [else
     (make-Constant rator)]))

(define (parse-application-rand rand)
  (cond
    [(expr? rand)
     (parse-expr rand)]
    [(seq? rand)
     (parse-seq rand)]
    [else
     (make-Constant rand)]))


(define (parse-branch expr)
  (match expr
    [(struct branch (test then else))
     (make-Branch (parse-expr-seq-constant test)
                  (parse-expr-seq-constant then)
                  (parse-expr-seq-constant else))]))


(define (parse-with-cont-mark expr)
  (match expr
    [(struct with-cont-mark (key val body))
     (make-WithContMark (parse-expr-seq-constant key)
                        (parse-expr-seq-constant val)
                        (parse-expr-seq-constant body))]))

(define (parse-beg0 expr)
  (match expr
    [(struct beg0 (seq))     
     (make-Begin0 (map parse-expr-seq-constant seq))]))


(define (parse-varref expr)
  (match expr
    [(struct varref (toplevel))
     (make-VariableReference (parse-toplevel toplevel))]))

(define (parse-assign expr)
  (match expr
    [(struct assign ((struct toplevel (depth pos const? ready?)) rhs undef-ok?))
     (make-ToplevelSet depth pos (parse-expr-seq-constant rhs))]))


(define (parse-apply-values expr)
  (match expr
    [(struct apply-values (proc args-expr))
     (make-ApplyValues (parse-expr-seq-constant proc)
                       (parse-expr-seq-constant args-expr))]))


(define (parse-primval expr)
  (match expr
    [(struct primval (id))
     (let ([name (hash-ref primitive-table id)])
       (make-PrimitiveKernelValue name))]))
