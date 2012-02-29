#lang typed/racket/base

(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/il-structs.rkt"
         racket/string)

(provide assemble-op-expression)



(: assemble-op-expression (PrimitiveOperator Blockht -> String))
(define (assemble-op-expression op blockht)
  (cond
    [(GetCompiledProcedureEntry? op)
     "M.p.label"]
    
    [(MakeCompiledProcedure? op)
     (cond
      ;; Small optimization: try to avoid creating the array if we know up front
      ;; that the closure has no closed values.
      [(null? (MakeCompiledProcedure-closed-vals op))
       (format "new RT.Closure(~a,~a,void(0),~a)"
               (assemble-label (make-Label (MakeCompiledProcedure-label op))
                               blockht)
               (assemble-arity (MakeCompiledProcedure-arity op))
               (assemble-display-name (MakeCompiledProcedure-display-name op)))]
      [else
       (format "new RT.Closure(~a,~a,[~a],~a)"
               (assemble-label (make-Label (MakeCompiledProcedure-label op))
                               blockht)
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
             (assemble-label (make-Label (MakeCompiledProcedureShell-label op))
                             blockht)
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
     (format "M.modules[~s].getNamespace().get(~s)"
             (symbol->string
              (ModuleLocator-name
               (ModuleVariable-module-name op)))
             (symbol->string (ModuleVariable-name op)))]

    [(PrimitivesReference? op)
     (format "M.primitives[~s]" (symbol->string (PrimitivesReference-name op)))]))