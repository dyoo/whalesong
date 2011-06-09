#lang typed/racket/base

(require "../compiler/compiler.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/compiler-structs.rkt"
         "../compiler/expression-structs.rkt"
         "../parameters.rkt"
         "../sets.rkt"
         "get-dependencies.rkt"
         "make-structs.rkt"
         racket/list
         racket/match)


(require/typed "../parser/parse-bytecode.rkt"
               [parse-bytecode (Any -> Expression)])

(require/typed "../get-module-bytecode.rkt"
               [get-module-bytecode ((U String Path Input-Port) -> Bytes)])


(provide make
         current-module-source-compiling-hook
         get-ast-and-statements)


(: current-module-source-compiling-hook
   (Parameterof (Source -> Source)))
(define current-module-source-compiling-hook
  (make-parameter (lambda: ([s : Source]) s)))





(: get-ast-and-statements (Source -> (values (U False Expression)
                                             (Listof Statement))))
(define (get-ast-and-statements a-source)
  (cond
   [(StatementsSource? a-source)
    (values #f (StatementsSource-stmts a-source))]

   [(UninterpretedSource? a-source)
    (values #f '())]
   
   [(MainModuleSource? a-source)
    (let-values ([(ast stmts)
                  (get-ast-and-statements (MainModuleSource-source a-source))])
      (let ([maybe-module-locator (find-module-locator ast)])
        (cond
         [(ModuleLocator? maybe-module-locator)
          (values ast (append stmts
                             ;; Set the main module name
                              (list (make-PerformStatement
                                     (make-AliasModuleAsMain!
                                      maybe-module-locator)))))]
         [else
          (values ast stmts)])))]

   [else
    (let ([ast
           (cond
            [(ModuleSource? a-source)
             (parse-bytecode (ModuleSource-path a-source))]
            [(SexpSource? a-source)
             (let ([source-code-op (open-output-bytes)])
               (write (SexpSource-sexp a-source) source-code-op)
               (parse-bytecode
                (open-input-bytes
                 (get-module-bytecode
                  (open-input-bytes
                   (get-output-bytes source-code-op))))))])])
      (values ast
              (compile ast 'val next-linkage/drop-multiple)))]))



(: find-module-locator ((U Expression False) -> (U False ModuleLocator)))
;; Tries to look for the module locator of this expression.
(define (find-module-locator exp)
  (match exp
    [(struct Top ((? Prefix?)
                  (struct Module (name
                                  (and path (? ModuleLocator?))
                                  prefix
                                  requires
                                  provides
                                  code))))
     path]
    [else
     #f]))




(: make ((Listof Source) Configuration -> Void))
(define (make sources config)
  (parameterize ([current-seen-unimplemented-kernel-primitives
                  ((inst new-seteq Symbol))])

    (match config
      [(struct Configuration (wrap-source
                              should-follow-children?
                              on-module-statements
                              after-module-statements
                              after-last))


       (: follow-dependencies ((Listof Source) -> Void))
       (define (follow-dependencies sources)
         (define visited ((inst make-hash Any Boolean)))

         (: collect-new-dependencies
            (Source (U False Expression) -> (Listof Source)))
         (define (collect-new-dependencies this-source ast)
           (cond
            [(eq? ast #f)
             empty]
            [(not (should-follow-children? this-source))
             empty]
            [else
             (let* ([dependent-module-names (get-dependencies ast)]
                    [paths
                     (foldl (lambda: ([mp : ModuleLocator]
                                      [acc : (Listof Source)])
                                     (let ([rp [ModuleLocator-real-path mp]])
                                       (cond [((current-kernel-module-locator?)
                                               mp)
                                              acc]
                                             [(path? rp)
                                              (cons (make-ModuleSource rp) acc)]
                                             [else
                                              acc])))
                            '()
                            dependent-module-names)])
               paths)]))
         
         (let: loop : Void ([sources : (Listof Source) sources])
           (cond
            [(empty? sources)
             (after-last)]
            [(hash-has-key? visited (first sources))
             (loop (rest sources))]
            [else
             (hash-set! visited (first sources) #t)
             (let*-values ([(this-source)
                             ((current-module-source-compiling-hook)
                              (first sources))]
                           [(ast stmts)
                            (get-ast-and-statements this-source)])
               (on-module-statements this-source ast stmts)
               (loop (append (map wrap-source (collect-new-dependencies this-source ast))
                             (rest sources)))
               (after-module-statements this-source ast stmts))])))

       (follow-dependencies (map wrap-source sources))])))
