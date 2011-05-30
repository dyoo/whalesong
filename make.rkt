#lang typed/racket/base

(require "compiler/compiler.rkt"
         "compiler/il-structs.rkt"
         "compiler/lexical-structs.rkt"
         "compiler/bootstrapped-primitives.rkt"
         "compiler/compiler-structs.rkt"
         "compiler/expression-structs.rkt"
         "get-dependencies.rkt"
         "parameters.rkt"
         "sets.rkt"
         "make-structs.rkt"
         racket/list
         racket/match)


(require/typed "parser/parse-bytecode.rkt"
               [parse-bytecode (Any -> Expression)])

(require/typed "get-module-bytecode.rkt"
               [get-module-bytecode ((U String Path Input-Port) -> Bytes)])


(provide make
         get-ast-and-statements)



(: get-ast-and-statements (Source -> (values (U False Expression)
                                             (Listof Statement))))
(define (get-ast-and-statements a-source)
  (cond
   [(StatementsSource? a-source)
    (values #f (StatementsSource-stmts a-source))]

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
                                  code))))
     path]
    [else
     #f]))




(: make ((Listof Source) Configuration -> Void))
(define (make sources config)
  (parameterize ([current-seen-unimplemented-kernel-primitives
                  ((inst new-seteq Symbol))])

    (match config
      [(struct Configuration (should-follow?
                              on-module-statements
                              after-module-statements
                              after-last))


       (: follow-dependencies ((Listof Source) -> Void))
       (define (follow-dependencies sources)
         (define visited ((inst make-hash Any Boolean)))

         (: collect-new-dependencies
            ((U False Expression) (Listof Source) -> (Listof Source)))
         (define (collect-new-dependencies ast sources)
           (cond
            [(eq? ast #f)
             sources]
            [else
             (let* ([dependent-module-names (get-dependencies ast)]
                    [paths
                     (foldl (lambda: ([mp : ModuleLocator]
                                      [acc : (Listof Source)])
                                     (let ([rp [ModuleLocator-real-path mp]])
                                       
                                       (cond [((current-kernel-module-locator?)
                                               mp)
                                              acc]
                                             [(and (path? rp)
                                                   (should-follow? rp)
                                                   (cons (make-ModuleSource rp)
                                                         acc))]
                                             [else
                                              acc])))
                            '()
                            dependent-module-names)])
               (append paths sources))]))
         
         (let: loop : Void ([sources : (Listof Source) sources])
           (cond
            [(empty? sources)
             (after-last)]
            [(hash-has-key? visited (first sources))
             (loop (rest sources))]
            [else
             (hash-set! visited (first sources) #t)
             (let-values ([(ast stmts)
                           (get-ast-and-statements (first sources))])
               (on-module-statements ast stmts)
               (loop (collect-new-dependencies ast (rest sources)))
               (after-module-statements ast stmts))])))

       (follow-dependencies sources)])))

