#lang typed/racket/base
(require "expression-structs.rkt"
         "il-structs.rkt"
         "lexical-structs.rkt"
         racket/list)

(provide optimize-il)

;; perform optimizations on the intermediate language.
;;



(: optimize-il ((Listof Statement) -> (Listof Statement)))
(define (optimize-il statements)

  #;statements
  ;; For now, replace pairs of PushEnvironment / AssignImmediate(0, ...)
  ;; We should do some more optimizations here, like peephole...
  (let loop ([statements (filter not-no-op? statements)])
    (cond
      [(empty? statements)
       empty]
      [else
       (let ([first-stmt (first statements)])
         (: default (-> (Listof Statement)))
         (define (default)
           (cons first-stmt
                 (loop (rest statements))))
         (cond
           [(empty? (rest statements))
            (default)]
           [else
            (let ([second-stmt (second statements)])
              (cond
                [(and (PushEnvironment? first-stmt)
                      (equal? first-stmt (make-PushEnvironment 1 #f))
                      (AssignImmediateStatement? second-stmt))
                 (let ([target (AssignImmediateStatement-target second-stmt)])
                   (cond
                     [(equal? target (make-EnvLexicalReference 0 #f))
                      (cons (make-PushImmediateOntoEnvironment 
                             (adjust-oparg-depth 
                              (AssignImmediateStatement-value second-stmt) -1)
                             #f)
                            (loop (rest (rest statements))))]
                     [else
                      (default)]))]
                [else
                 (default)]))]))])))
       

(: not-no-op? (Statement -> Boolean))
(define (not-no-op? stmt) (not (no-op? stmt)))


(: no-op? (Statement -> Boolean))
;; Produces true if the statement should have no effect.
(define (no-op? stmt)
  (cond
    [(symbol? stmt)
     #f]
    
    [(LinkedLabel? stmt)
     #f]
    
    [(DebugPrint? stmt)
     #f]
    
    [(AssignImmediateStatement? stmt)
     (equal? (AssignImmediateStatement-target stmt)
             (AssignImmediateStatement-value stmt))]

    [(AssignPrimOpStatement? stmt)
     #f]
    
    [(PerformStatement? stmt)
     #f]
    
    [(GotoStatement? stmt)
     #f]
    
    [(TestAndBranchStatement? stmt)
     #f]
    
    [(PopEnvironment? stmt)
     (and (Const? (PopEnvironment-n stmt))
          (equal? (PopEnvironment-n stmt) 
                  (make-Const 0)))]
     
    [(PushEnvironment? stmt)
     (= (PushEnvironment-n stmt) 0)]
    
    [(PushImmediateOntoEnvironment? stmt)
     #f]

    [(PushControlFrame/Generic? stmt)
     #f]
    
    [(PushControlFrame/Call? stmt)
     #f]
    
    [(PushControlFrame/Prompt? stmt)
     #f]
    
    [(PopControlFrame? stmt)
     #f]
    [(Comment? stmt)
     #f]))





(: adjust-oparg-depth (OpArg Integer -> OpArg))
(define (adjust-oparg-depth oparg n)
  (cond
    [(Const? oparg) oparg]
    [(Label? oparg) oparg]
    [(Reg? oparg) oparg]
    [(EnvLexicalReference? oparg)
     (make-EnvLexicalReference (ensure-natural (+ n (EnvLexicalReference-depth oparg)))
                               (EnvLexicalReference-unbox? oparg))]
    [(EnvPrefixReference? oparg)
     (make-EnvPrefixReference (ensure-natural (+ n (EnvPrefixReference-depth oparg)))
                              (EnvPrefixReference-pos oparg))]
    [(EnvWholePrefixReference? oparg)
     (make-EnvWholePrefixReference (ensure-natural (+ n (EnvWholePrefixReference-depth oparg))))]
    [(SubtractArg? oparg)
     (make-SubtractArg (adjust-oparg-depth (SubtractArg-lhs oparg) n)
                       (adjust-oparg-depth (SubtractArg-rhs oparg) n))]
    [(ControlStackLabel? oparg)
     oparg]
    [(ControlStackLabel/MultipleValueReturn? oparg)
     oparg]
    [(ControlFrameTemporary? oparg)
     oparg]
    [(CompiledProcedureEntry? oparg)
     (make-CompiledProcedureEntry (adjust-oparg-depth (CompiledProcedureEntry-proc oparg) n))]
    [(CompiledProcedureClosureReference? oparg)
     (make-CompiledProcedureClosureReference 
      (adjust-oparg-depth (CompiledProcedureClosureReference-proc oparg) n)
      (CompiledProcedureClosureReference-n oparg))]
    [(PrimitiveKernelValue? oparg)
     oparg]
    [(ModuleEntry? oparg)
     oparg]
    [(IsModuleInvoked? oparg)
     oparg]
    [(IsModuleLinked? oparg)
     oparg]
    [(VariableReference? oparg)
     (let ([t (VariableReference-toplevel oparg)])
       (make-VariableReference 
        (make-ToplevelRef (ensure-natural (+ n (ToplevelRef-depth t)))
                          (ToplevelRef-pos t))))]))


(define-predicate natural? Natural)
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))