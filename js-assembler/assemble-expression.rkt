#lang typed/racket/base

(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         "../compiler/il-structs.rkt"
         racket/string)

(provide assemble-op-expression)



(: assemble-op-expression (PrimitiveOperator -> String))
(define (assemble-op-expression op)
  (cond
    [(GetCompiledProcedureEntry? op)
     "M.proc.label"]
    
    [(MakeCompiledProcedure? op)
     (format "new RT.Closure(~a, ~a, [~a], ~a)"
             (assemble-label (make-Label (MakeCompiledProcedure-label op)))
             (assemble-arity (MakeCompiledProcedure-arity op))
             (string-join (map
			   assemble-env-reference/closure-capture 
                               ;; The closure values are in reverse order
                               ;; to make it easier to push, in bulk, into
                               ;; the environment (which is also in reversed order)
                               ;; during install-closure-values.
                               (reverse (MakeCompiledProcedure-closed-vals op)))
                          ", ")
             (assemble-display-name (MakeCompiledProcedure-display-name op)))]
    
    [(MakeCompiledProcedureShell? op)
     (format "new RT.Closure(~a, ~a, undefined, ~a)"
             (assemble-label (make-Label (MakeCompiledProcedureShell-label op)))
             (assemble-arity (MakeCompiledProcedureShell-arity op))
             (assemble-display-name (MakeCompiledProcedureShell-display-name op)))]
    
    [(ApplyPrimitiveProcedure? op)
     (format "M.proc(M)")]
    
    [(CaptureEnvironment? op)
     (format "M.env.slice(0, M.env.length - ~a)"
             (CaptureEnvironment-skip op))]
    
    [(CaptureControl? op)
     (format "M.captureControl(~a, ~a)"
             (CaptureControl-skip op)
             (let: ([tag : (U DefaultContinuationPromptTag OpArg)
			 (CaptureControl-tag op)])
               (cond [(DefaultContinuationPromptTag? tag)
                      (assemble-default-continuation-prompt-tag)]
                     [(OpArg? tag)
                      (assemble-oparg tag)])))]

    
    [(MakeBoxedEnvironmentValue? op)
     (format "[M.env[M.env.length - 1 - ~a]]"
             (MakeBoxedEnvironmentValue-depth op))]

    [(CallKernelPrimitiveProcedure? op)
     (open-code-kernel-primitive-procedure op)]))