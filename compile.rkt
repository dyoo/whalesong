#lang typed/racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         "find-toplevel-variables.rkt"
         "sets.rkt"
         racket/list)

(provide compile-top)





(: compile (ExpressionCore CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Compiles an expression into an instruction sequence.
(define (compile exp cenv target linkage)
  (cond
    [(Top? exp)
     (compile-top exp cenv target linkage)]
    [(Constant? exp)
     (compile-constant exp cenv target linkage)]
    [(Var? exp)
     (compile-variable exp cenv target linkage)]
    [(Def? exp)
     (compile-definition exp cenv target linkage)]
    [(Branch? exp)
     (compile-branch exp cenv target linkage)]
    [(Lam? exp)
     (compile-lambda exp cenv target linkage)]
    [(Seq? exp)
     (compile-sequence (Seq-actions exp)
                       cenv
                       target
                       linkage)]
    [(App? exp)
     (compile-application exp cenv target linkage)]))



(: compile-top (Top CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-top top cenv target linkage)
  (let*: ([cenv : CompileTimeEnvironment (extend-lexical-environment cenv (Top-prefix top))]
          [names : (Listof Symbol) (Prefix-names (Top-prefix top))])
        (append-instruction-sequences
         (make-instruction-sequence 
          `(,(make-PerformStatement 'extend-environment/prefix!
                                    (list (make-Const names)))))
         (compile (Top-code top) cenv target linkage))))



(: compile-linkage (CompileTimeEnvironment Linkage -> InstructionSequence))
(define (compile-linkage cenv linkage)
  (cond
    [(eq? linkage 'return)
     (make-instruction-sequence `(,(make-AssignPrimOpStatement 'proc
                                                               'read-control-label
                                                               (list))
                                  ,(make-PopEnv (lexical-environment-pop-depth cenv)
                                                ;; FIXME: not right
                                                0)
                                  ,(make-PopControl)
                                  ,(make-GotoStatement (make-Reg 'proc))))]
    [(eq? linkage 'next)
     empty-instruction-sequence]
    [(symbol? linkage)
     (make-instruction-sequence `(,(make-GotoStatement (make-Label linkage))))]))

(: end-with-linkage (Linkage CompileTimeEnvironment InstructionSequence ->
                             InstructionSequence))
(define (end-with-linkage linkage cenv instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-linkage cenv linkage)))

(: compile-constant (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-constant exp cenv target linkage)
  (end-with-linkage linkage
                    cenv
                    (make-instruction-sequence
                     `(,(make-AssignImmediateStatement target (make-Const (Constant-v exp)))))))

(: compile-variable (Var CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-variable exp cenv target linkage)
  (let ([lexical-pos (find-variable (Var-id exp) cenv)])
    (cond
      [(LocalAddress? lexical-pos)
       (end-with-linkage linkage
                         cenv
                         (make-instruction-sequence
                          `(,(make-AssignPrimOpStatement target
                                                         'lexical-address-lookup
                                                         (list (make-Const 
                                                                (LocalAddress-depth lexical-pos))
                                                               (make-Reg 'env))))))]
      [(PrefixAddress? lexical-pos)
       (end-with-linkage linkage
                         cenv
                         (make-instruction-sequence
                          `(,(make-PerformStatement 'check-bound! 
                                                    (list (make-Const (PrefixAddress-depth lexical-pos))
                                                          (make-Const (PrefixAddress-pos lexical-pos))
                                                          (make-Const (PrefixAddress-name lexical-pos))))
                            ,(make-AssignPrimOpStatement target
                                                         'toplevel-lookup
                                                         (list (make-Const (PrefixAddress-depth lexical-pos))
                                                               (make-Const (PrefixAddress-pos lexical-pos))
                                                               (make-Const (PrefixAddress-name lexical-pos))
                                                               (make-Reg 'env))))))])))


(: compile-definition (Def CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-definition exp cenv target linkage)
  (let* ([var (Def-variable exp)]
         [lexical-pos (find-variable var cenv)]
         [get-value-code
          (compile (Def-value exp) cenv 'val 'next)])
    (cond
      [(LocalAddress? lexical-pos)
       (error 'compile-definition "Defintion not at toplevel")]
      [(PrefixAddress? lexical-pos)
       (end-with-linkage
        linkage
        cenv
        (append-instruction-sequences
         get-value-code
         (make-instruction-sequence `(,(make-PerformStatement 'toplevel-set!
                                                              (list (make-Const (PrefixAddress-depth lexical-pos))
                                                                    (make-Const (PrefixAddress-pos lexical-pos))
                                                                    (make-Const var)))
                                      ,(make-AssignImmediateStatement target (make-Const 'ok))))))])))


(: compile-branch (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-branch exp cenv target linkage)  
  (let ([t-branch (make-label 'trueBranch)]
        [f-branch (make-label 'falseBranch)]
        [after-if (make-label 'afterIf)])
    (let ([consequent-linkage
           (if (eq? linkage 'next)
               after-if
               linkage)])
      (let ([p-code (compile (Branch-predicate exp) cenv 'val 'next)]
            [c-code (compile (Branch-consequent exp) cenv target consequent-linkage)]
            [a-code (compile (Branch-alternative exp) cenv target linkage)])
        (append-instruction-sequences p-code
                                      (append-instruction-sequences
                                       (make-instruction-sequence
                                        `(,(make-TestStatement 'false? 'val)
                                          ,(make-BranchLabelStatement f-branch)))
                                       (append-instruction-sequences
                                        (append-instruction-sequences t-branch c-code)
                                        (append-instruction-sequences f-branch a-code))
                                       after-if))))))


(: compile-sequence ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-sequence seq cenv target linkage) 
  ;; All but the last will use 'next linkage.
  (if (last-exp? seq)
      (compile (first-exp seq) cenv target linkage)
      (append-instruction-sequences (compile (first-exp seq) cenv target 'next)
                                    (compile-sequence (rest-exps seq) cenv target linkage))))


(: compile-lambda (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
;; Write out code for lambda expressions.
;; The lambda will close over the free variables.
(define (compile-lambda exp cenv target linkage) 
  (let*: ([proc-entry : Symbol (make-label 'entry)]
          [after-lambda : Symbol (make-label 'afterLambda)]
          [lambda-linkage : Linkage
                          (if (eq? linkage 'next)
                              after-lambda
                              linkage)]
          [free-vars : (Listof Symbol) (find-toplevel-variables exp)]
          [lexical-addresses : (Listof LexicalAddress) 
                             (map (lambda: ([var : Symbol])
                                           (find-variable var cenv))
                                  free-vars)]
          [lexical-references : (Listof (U EnvLexicalReference EnvWholePrefixReference))
                              (collect-lexical-references lexical-addresses)])
         (append-instruction-sequences
          (end-with-linkage lambda-linkage
                            cenv
                            (make-instruction-sequence 
                             `(,(make-AssignPrimOpStatement target
                                                            'make-compiled-procedure
                                                            (list* (make-Label proc-entry)
                                                                   lexical-references)))))
          (compile-lambda-body exp cenv
                               lexical-references
                               proc-entry)
          after-lambda)))




  

(: compile-lambda-body (Lam CompileTimeEnvironment 
                            (Listof (U EnvLexicalReference
                                       EnvWholePrefixReference))
                            Linkage 
                            -> 
                            InstructionSequence))
;; Compiles the body of the lambda in the appropriate environment.
(define (compile-lambda-body exp cenv lexical-references proc-entry)
  (let*: ([formals : (Listof Symbol) (Lam-parameters exp)]
          [extended-cenv : CompileTimeEnvironment 
                         (extend-lexical-environment '() formals)]
          [extended-cenv : CompileTimeEnvironment 
                         (lexical-references->compile-time-environment 
                          lexical-references cenv extended-cenv)])
         (append-instruction-sequences
          (make-instruction-sequence 
           `(,proc-entry
             ,(make-PerformStatement 'install-closure-values!
                                     (list (make-Reg 'proc)))))
          (compile (Lam-body exp) extended-cenv 'val 'return))))



;; FIXME: I need to implement important special cases.
;; 1. We may be able to open-code if the operator is primitive
;; 2. We may have a static location to jump to if the operator is lexically scoped.
(: compile-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-application exp cenv target linkage) 
  (let* ([extended-cenv (extend-lexical-environment/placeholders cenv (length (App-operands exp)))]
         [proc-code (compile (App-operator exp)
                             extended-cenv 
                             (if (empty? (App-operands exp))
                                 'proc
                                 (make-EnvOffset (max 0 (sub1 (length (App-operands exp))))))
                             'next)]
         [operand-codes (map (lambda: ([operand : Expression]
                                       [target : Target])
                                      (compile operand extended-cenv target 'next))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda: ([i : Natural])
                                                  (if (< i (sub1 (length (App-operands exp))))
                                                      (make-EnvOffset i)
                                                      'val))))])

    ;; FIXME: we need to push the control.
    ;; FIXME: at procedure entry, the arguments need to be installed
    ;; in the environment.  We need to install 
    ;; the closure's values now.
    (append-instruction-sequences
     (make-instruction-sequence `(,(make-PushEnv (length (App-operands exp)))))
     proc-code
     (juggle-operands operand-codes)
     (compile-procedure-call extended-cenv (length (App-operands exp)) target linkage))))
    


(: juggle-operands ((Listof InstructionSequence) -> InstructionSequence))
;; Installs the operators.  At the end of this,
;; the procedure lives in 'proc, and the operands on the environment stack.
(define (juggle-operands operand-codes)
  (let: ([n : Natural 
            ;; defensive coding: the operand codes should be nonempty.
            (max 0 (sub1 (length operand-codes)))])
        (let: loop : InstructionSequence ([ops : (Listof InstructionSequence) operand-codes])
              (cond
                ;; If there are no operands, no need to juggle.
                [(null? ops)
                 (make-instruction-sequence empty)]
                [(null? (rest ops))
                 ;; The last operand needs to be handled specially: it currently lives in 
                 ;; val.  We move the procedure at env[n] over to proc, and move the
                 ;; last operand at 'val into env[n].
                 (make-instruction-sequence 
                  `(,(make-AssignImmediateStatement 'proc 
                                                    (make-EnvLexicalReference n))
                    ,(make-AssignImmediateStatement (make-EnvOffset n)
                                                    (make-Reg 'val))))]
                [else
                 ;; Otherwise, add instructions to juggle the operator and operands in the stack.
                 (append-instruction-sequences (car ops)
                                               (loop (rest ops)))]))))



(: compile-procedure-call (CompileTimeEnvironment Natural Target Linkage -> InstructionSequence))
;; Assumes the procedure value has been loaded into the proc register.
(define (compile-procedure-call cenv n target linkage)
  (let ([primitive-branch (make-label 'primitiveBranch)]
        [compiled-branch (make-label 'compiledBranch)]
        [after-call (make-label 'afterCall)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      
      (append-instruction-sequences
       (make-instruction-sequence `(,(make-TestStatement 'primitive-procedure? 'proc)
                                    ,(make-BranchLabelStatement primitive-branch)))        
       
       compiled-branch
       (compile-proc-appl n target compiled-linkage)

       primitive-branch
       (end-with-linkage linkage
                         cenv
                         (make-instruction-sequence 
                          `(,(make-AssignPrimOpStatement target
                                                         'apply-primitive-procedure
                                                         (list (make-Reg 'proc)
                                                               (make-Const n)
                                                               (make-Reg 'env))))))
       after-call))))



(: compile-proc-appl (Natural Target Linkage -> InstructionSequence))
;; Three fundamental cases for general compiled-procedure application.
;;    1.  Non-tail calls that write to val
;;    2.  Calls in argument position that write to the environment
;;    3.  Tail calls.
;; The Other cases should be excluded.
(define (compile-proc-appl n target linkage)
  (cond [(eq? linkage 'next)
         ;; This case should be impossible: next linkage can't be used in this position.
         (error 'compile "next linkage")]
        
        [(and (eq? target 'val)
              (not (eq? linkage 'return)))
         ;; This case happens for a function call that isn't in
         ;; tail position.
         (make-instruction-sequence 
          `(,(make-PushControlFrame linkage)
            ,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                         (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]
        
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         ;; This case happens for evaluating arguments, since the
         ;; arguments are being installed into the scratch space.
         (let ([proc-return (make-label 'procReturn)])
           (make-instruction-sequence
            `(,(make-PushControlFrame proc-return)
              ,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                           (list (make-Reg 'proc)))
              ,(make-GotoStatement (make-Reg 'val))
              ,proc-return
              ,(make-AssignImmediateStatement target (make-Reg 'val))
              ,(make-GotoStatement (make-Label linkage)))))]
        
        [(and (eq? target 'val)
              (eq? linkage 'return))
         ;; This case happens when we're in tail position.
         ;; FIXME: do tail call stuff!
         ;; Must shift existing environment to replace
         (make-instruction-sequence
          `(,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                         (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]

        [(and (not (eq? target 'val))
              (eq? linkage 'return))
         ;; This case should be impossible: return linkage should only
         ;; occur when we're in tail position, and we're in tail position
         ;; only when the target is the val register.
         (error 'compile "return linkage, target not val: ~s" target)]))





(: append-instruction-sequences (InstructionSequence * -> InstructionSequence))
(define (append-instruction-sequences . seqs)
  (append-seq-list seqs))

(: append-2-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (append (statements seq1) (statements seq2))))

(: append-seq-list ((Listof InstructionSequence) -> InstructionSequence))
(define (append-seq-list seqs)
  (if (null? seqs)
      empty-instruction-sequence
      (append-2-sequences (car seqs)
                          (append-seq-list (cdr seqs)))))
