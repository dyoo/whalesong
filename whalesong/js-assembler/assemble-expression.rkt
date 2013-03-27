#lang typed/racket/base

(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/il-structs.rkt"
         racket/string)

(provide assemble-op-expression
         current-interned-constant-closure-table
         assemble-current-interned-constant-closure-table)




(: current-interned-constant-closure-table (Parameterof (HashTable Symbol MakeCompiledProcedure)))
(define current-interned-constant-closure-table
  (make-parameter ((inst make-hasheq Symbol MakeCompiledProcedure))))


(: assemble-current-interned-constant-closure-table (-> String))
(define (assemble-current-interned-constant-closure-table)
  (string-join (hash-map
                  (current-interned-constant-closure-table)
                  (lambda: ([a-label : Symbol] [a-shell : MakeCompiledProcedure]) 
                           (format "var ~a_c=new RT.Closure(~a,~a,void(0),~a);"
                                   (assemble-label (make-Label (MakeCompiledProcedure-label a-shell)))
                                   (assemble-label (make-Label (MakeCompiledProcedure-label a-shell)))
                                   (assemble-arity (MakeCompiledProcedure-arity a-shell))
                                   (assemble-display-name (MakeCompiledProcedure-display-name a-shell)))))
                 "\n"))


(: assemble-op-expression (PrimitiveOperator Blockht -> String))
(define (assemble-op-expression op blockht)
  (cond
    [(GetCompiledProcedureEntry? op)
     "M.p.label"]
    
    [(MakeCompiledProcedure? op)
     (cond
      ;; Small optimization: try to avoid creating the array if we know up front
      ;; that the closure has no closed values.  It's a constant that we lift up to the toplevel.
      [(null? (MakeCompiledProcedure-closed-vals op))
       (define assembled-label (assemble-label (make-Label (MakeCompiledProcedure-label op))))
       (unless (hash-has-key? (current-interned-constant-closure-table) (MakeCompiledProcedure-label op))
         (hash-set! (current-interned-constant-closure-table)
                    (MakeCompiledProcedure-label op)
                    op))
       (format "~a_c" assembled-label)]
      [else
       (format "new RT.Closure(~a,~a,[~a],~a)"
               (assemble-label (make-Label (MakeCompiledProcedure-label op)))
               (assemble-arity (MakeCompiledProcedure-arity op))
               (string-join (map
                             assemble-env-reference/closure-capture 
                             ;; The closure values are in reverse order
                             ;; to make it easier to push, in bulk, into
                             ;; the environment (which is also in reversed order)
                             ;; during install-closure-values.
                             (reverse (MakeCompiledProcedure-closed-vals op)))
                            ",")
               (assemble-display-name (MakeCompiledProcedure-display-name op)))])]
    
    [(MakeCompiledProcedureShell? op)
     (format "new RT.Closure(~a,~a,void(0),~a)"
             (assemble-label (make-Label (MakeCompiledProcedureShell-label op)))
             (assemble-arity (MakeCompiledProcedureShell-arity op))
             (assemble-display-name (MakeCompiledProcedureShell-display-name op)))]
    
    [(CaptureEnvironment? op)
     (format "M.e.slice(0, M.e.length-~a)"
             (CaptureEnvironment-skip op))]
    
    [(CaptureControl? op)
     (format "M.captureControl(~a,~a)"
             (CaptureControl-skip op)
             (let: ([tag : (U DefaultContinuationPromptTag OpArg)
			 (CaptureControl-tag op)])
               (cond [(DefaultContinuationPromptTag? tag)
                      (assemble-default-continuation-prompt-tag)]
                     [(OpArg? tag)
                      (assemble-oparg tag blockht)])))]

    
    [(MakeBoxedEnvironmentValue? op)
     (format "[M.e[M.e.length-~a]]"
             (add1 (MakeBoxedEnvironmentValue-depth op)))]

    [(CallKernelPrimitiveProcedure? op)
     (open-code-kernel-primitive-procedure op blockht)]

    [(ApplyPrimitiveProcedure? op)
     (format "M.primitives[~s]._i(M)" (symbol->string (ApplyPrimitiveProcedure-name op)))]

    [(ModuleVariable? op)
     (format "M.modules[~s].getExports().get(~s)"
             (symbol->string
              (ModuleLocator-name
               (ModuleVariable-module-name op)))
             (symbol->string (ModuleVariable-name op)))]

    [(PrimitivesReference? op)
     (format "M.primitives[~s]" (symbol->string (PrimitivesReference-name op)))]

    [(GlobalsReference? op)
     (format "(M.globals[~s]!==void(0)?M.globals[~s]:M.params.currentNamespace.get(~s))" 
             (symbol->string (GlobalsReference-name op))
             (symbol->string (GlobalsReference-name op))
             (symbol->string (GlobalsReference-name op)))]))
