#lang typed/racket/base
(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         "find-toplevel-variables.rkt"
         "sets.rkt"
         "compile.rkt"
         racket/list)

(provide call/cc-label
         make-call/cc-code)



;; The primitive code necessary to do call/cc

(: call/cc-label Symbol)
(define call/cc-label 'callCCEntry)
(define call/cc-closure-entry 'callCCClosureEntry)


;; (call/cc f)
;; Tail-calls f, providing it a special object that knows how to do the low-level
;; manipulation of the environment and control stack.
(define (make-call/cc-code)
  (statements
   (append-instruction-sequences
    (make-instruction-sequence 
     `(,call/cc-label
       ;; Precondition: the environment holds the f function that we want to jump into.
       
       ;; First, move f to the proc register
       ,(make-AssignImmediateStatement 'proc (make-EnvLexicalReference 0 #f))
       
       ;; Next, capture the envrionment and the current continuation closure,.
       ,(make-PushEnvironment 2 #f)
       ,(make-AssignPrimOpStatement (make-EnvLexicalReference 0 #f) 
                                    (make-CaptureControl 0))
       ,(make-AssignPrimOpStatement (make-EnvLexicalReference 1 #f)
                                    ;; When capturing, skip over f and the two slots we just added.
                                    (make-CaptureEnvironment 3))
       ,(make-AssignPrimOpStatement (adjust-target-depth (make-EnvLexicalReference 0 #f) 2)
                                    (make-MakeCompiledProcedure call/cc-closure-entry
                                                                1 ;; the continuation consumes a single value
                                                                (list (make-EnvLexicalReference 0 #f)
                                                                      (make-EnvLexicalReference 1 #f))
                                                                'call/cc))
       ,(make-PopEnvironment 2 0)))
    
    ;; Finally, do a tail call into f.
    (compile-procedure-call '()
                            (extend-lexical-environment/placeholders '() 1)
                            1 
                            'val
                            'return)
    
    ;; The code for the continuation coe follows.  It's supposed to
    ;; abandon the current continuation, initialize the control and environment, and then jump.
    (make-instruction-sequence `(,call/cc-closure-entry
                                 ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
                                 ,(make-PerformStatement (make-InstallClosureValues!))
                                 ,(make-PerformStatement (make-RestoreControl!))
                                 ,(make-PerformStatement (make-RestoreEnvironment!))
                                 ,(make-AssignPrimOpStatement 'proc (make-GetControlStackLabel))
                                 ,(make-PopControlFrame)
                                 ,(make-GotoStatement (make-Reg 'proc)))))))
