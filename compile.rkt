#lang typed/racket/base

(require "typed-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         "find-toplevel-variables.rkt"
         racket/list)

(provide compile-top)


;; registers: env, argl, proc, val, cont
;; as well as the stack.
(define all-regs '(val control env))


(: compile-top (Expression Target Linkage -> InstructionSequence))
(define (compile-top exp target linkage)
  (let*: ([names : (Listof Symbol) (find-toplevel-variables exp)]
          [cenv : CompileTimeEnvironment (list (make-Prefix names))])
        (append-instruction-sequences
         (make-instruction-sequence 
          `(,(make-AssignPrimOpStatement 'env
                                         'extend-environment/prefix
                                         (list (make-Const names)
                                               (make-Reg 'env)))))
         (compile exp cenv target linkage))))


;; compile: expression target linkage -> instruction-sequence
(: compile (Expression CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile exp cenv target linkage)
  (cond
    [(Constant? exp)
     (compile-self-evaluating exp cenv target linkage)]
    #;[(Quote? exp)
     (compile-quoted exp cenv target linkage)]
    #;[(Var? exp)
     (compile-variable exp cenv target linkage)]
    #;[(Assign? exp)
     (compile-assignment exp cenv target linkage)]
    #;[(Def? exp)
     (compile-definition exp cenv target linkage)]
    #;[(Branch? exp)
     (compile-if exp cenv target linkage)]
    #;[(Lam? exp)
     (compile-lambda exp cenv target linkage)]
    #;[(Seq? exp)
     (compile-sequence (Seq-actions exp)
                       cenv
                       target
                       linkage)]
    #;[(App? exp)
     (compile-application exp cenv target linkage)]))



(: compile-linkage (Linkage -> InstructionSequence))
(define (compile-linkage linkage)
  (cond
    #;[(eq? linkage 'return)
       (make-instruction-sequence `(,(make-GotoStatement (make-ControlOffset 0))))]
    [(eq? linkage 'next)
     empty-instruction-sequence]
    [else
     (make-instruction-sequence `(,(make-GotoStatement (make-Label linkage))))]))

(: end-with-linkage (Linkage InstructionSequence -> InstructionSequence))
(define (end-with-linkage linkage instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-linkage linkage)))

(: compile-self-evaluating (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-self-evaluating exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     `(,(make-AssignImmediateStatement target (make-Const (Constant-v exp)))))))

#;(: compile-quoted (Quote CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-quoted exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     `(,(make-AssignImmediateStatement target (make-Const (Quote-text exp)))))))

#;(: compile-variable (Var CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-variable exp cenv target linkage)
  (let ([lexical-pos (find-variable (Var-id exp) cenv)])
    (cond
      [(LocalAddress? lexical-pos)
       (end-with-linkage linkage
                         (make-instruction-sequence
                          `(,(make-AssignPrimOpStatement target
                                                         'lexical-address-lookup
                                                         (list (make-Const (LocalAddress-depth lexical-pos))
                                                               (make-Const (LocalAddress-pos lexical-pos))
                                                               (make-Reg 'env))))))]
      [(PrefixAddress? lexical-pos)
       (end-with-linkage linkage
                         (make-instruction-sequence
                          `(,(make-PerformStatement 'check-bound! 
                                                    (list (make-Const (PrefixAddress-depth lexical-pos))
                                                          (make-Const (PrefixAddress-pos lexical-pos))
                                                          (make-Const (PrefixAddress-name lexical-pos))
                                                          (make-Reg 'env)))
                            ,(make-AssignPrimOpStatement target
                                                         'toplevel-lookup
                                                         (list (make-Const (PrefixAddress-depth lexical-pos))
                                                               (make-Const (PrefixAddress-pos lexical-pos))
                                                               (make-Const (PrefixAddress-name lexical-pos))
                                                               (make-Reg 'env))))))])))


#;(: compile-assignment (Assign CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-assignment exp cenv target linkage)
  (let* ([var (Assign-variable exp)]
         [get-value-code
          (compile (Assign-value exp) cenv 'val 'next)]
         [lexical-address
          (find-variable var cenv)])
    (cond
      [(LocalAddress? lexical-address)
       (end-with-linkage
        linkage
        (append-instruction-sequences get-value-code
                                      (make-instruction-sequence
                                       `(,(make-PerformStatement 'lexical-address-set!
                                                                 (list (make-Const (LocalAddress-depth lexical-address))
                                                                       (make-Const (LocalAddress-pos lexical-address))
                                                                       (make-Reg 'env)
                                                                       (make-Reg 'val)))
                                         ,(make-AssignImmediateStatement target (make-Const 'ok))))))]
      [(PrefixAddress? lexical-address)
       (end-with-linkage
        linkage
        (append-instruction-sequences get-value-code
                                      (make-instruction-sequence
                                       `(,(make-PerformStatement 'toplevel-set!
                                                                 (list (make-Const (PrefixAddress-depth lexical-address))
                                                                       (make-Const (PrefixAddress-pos lexical-address))
                                                                       (make-Const (PrefixAddress-name lexical-address))
                                                                       (make-Reg 'env)
                                                                       (make-Reg 'val)))
                                         ,(make-AssignImmediateStatement target (make-Const 'ok))))))])))


#;(: compile-definition (Def CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-definition exp cenv target linkage)
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
        (append-instruction-sequences
         get-value-code
         (make-instruction-sequence `(,(make-PerformStatement 'toplevel-set!
                                                              (list (make-Const (PrefixAddress-depth lexical-pos))
                                                                    (make-Const (PrefixAddress-pos lexical-pos))
                                                                    (make-Const var)
                                                                    (make-Reg 'env)
                                                                    (make-Reg 'val)))
                                      ,(make-AssignImmediateStatement target (make-Const 'ok))))))])))


#;(: compile-if (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-if exp cenv target linkage)  
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
                                       (parallel-instruction-sequences
                                        (append-instruction-sequences t-branch c-code)
                                        (append-instruction-sequences f-branch a-code))
                                       after-if))))))


#;(: compile-sequence ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-sequence seq cenv target linkage) 
  (if (last-exp? seq)
      (compile (first-exp seq) cenv target linkage)
      (append-instruction-sequences (compile (first-exp seq) cenv target 'next)
                                    (compile-sequence (rest-exps seq) cenv target linkage))))


#;(: compile-lambda (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-lambda exp cenv target linkage) 
  (let ([proc-entry (make-label 'entry)]
        [after-lambda (make-label 'afterLambda)])
    (let ([lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)])
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence 
                           `(,(make-AssignPrimOpStatement target
                                                          'make-compiled-procedure
                                                          (list (make-Label proc-entry)
                                                                ;; TODO: rather than capture the whole
                                                                ;; environment, we may instead
                                                                ;; just capture the free variables.
                                                                ;; But that requires that we box
                                                                ;; up all set!-ed variables, in order
                                                                ;; to preserve semantics of set!
                                                                (make-Reg 'env))))))
        (compile-lambda-body exp cenv
                             proc-entry))
       after-lambda))))

#;(: compile-lambda-body (Lam CompileTimeEnvironment Linkage -> InstructionSequence))
#;(define (compile-lambda-body exp cenv proc-entry)
  (let* ([formals (Lam-parameters exp)]
         [extended-cenv (extend-lexical-environment cenv formals)])
    (append-instruction-sequences
     (make-instruction-sequence 
      `(,proc-entry
        ,(make-AssignPrimOpStatement 'env 
                                     'compiled-procedure-env
                                     (list (make-Reg 'proc)))
        ,(make-AssignPrimOpStatement 'env
                'extend-environment
                (list (make-Reg 'argl)
                      (make-Reg 'env)))))
     (compile-sequence (Lam-body exp) extended-cenv 'val 'return))))

#;(: compile-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-application exp cenv target linkage) 
  ;; FIXME: I need to implement important special cases.
  ;; 1. We may be able to open-code if the operator is primitive
  ;; 2. We may have a static location to jump to if the operator is lexically scoped.
  (let ([proc-code (compile (App-operator exp) cenv 'proc 'next)]
        [operand-codes (map (lambda: ([operand : Expression])
                              (compile operand cenv 'val 'next))
                            (App-operands exp))])
    (append-instruction-sequences
     proc-code
     (append-instruction-sequences
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

#;(: construct-arglist ((Listof InstructionSequence) -> InstructionSequence))
#;(define (construct-arglist operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (null? operand-codes)
        (make-instruction-sequence `(,(make-AssignImmediateStatement 'argl (make-Const '()))))
        (let ([code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence `(,(make-AssignPrimOpStatement 'argl 'list 
                                                                          (list (make-Reg 'val))))))])
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (append-instruction-sequences code-to-get-last-arg
                                            (code-to-get-rest-args
                                             (cdr operand-codes))))))))
#;(: code-to-get-rest-args ((Listof InstructionSequence) -> InstructionSequence))
#;(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (append-instruction-sequences
          (car operand-codes)
          (make-instruction-sequence 
           `(,(make-AssignPrimOpStatement 'argl 
                                          'cons 
                                          (list (make-Reg 'val)
                                                (make-Reg 'argl))))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (append-instruction-sequences code-for-next-arg
                                      (code-to-get-rest-args (cdr operand-codes))))))

#;(: compile-procedure-call (Target Linkage -> InstructionSequence))
#;(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-label 'primitiveBranch)]
        [compiled-branch (make-label 'compiledBranch)]
        [after-call (make-label 'afterCall)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence `(,(make-TestStatement 'primitive-procedure? 'proc)
                                    ,(make-BranchLabelStatement primitive-branch)))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence 
                            `(,(make-AssignPrimOpStatement target
                                                           'apply-primitive-procedure
                                                           (list (make-Reg 'proc)
                                                                 (make-Reg 'argl))))))))
       after-call))))

#;(: compile-proc-appl (Target Linkage -> InstructionSequence))
#;(define (compile-proc-appl target linkage)
  (cond [(and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          `(,(make-AssignImmediateStatement 'cont (make-Label linkage))
            ,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                         (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ([proc-return (make-label 'procReturn)])
           (make-instruction-sequence
            `(,(make-AssignImmediateStatement 'cont (make-Label proc-return))
              ,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                           (list (make-Reg 'proc)))
              ,(make-GotoStatement (make-Reg 'val))
              ,proc-return
              ,(make-AssignImmediateStatement target (make-Reg 'val))
              ,(make-GotoStatement (make-Label linkage)))))]
        [(and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          `(,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                        (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]
        [(and (not (eq? target 'val))
              (eq? linkage 'return))
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




(: tack-on-instruction-sequence (InstructionSequence InstructionSequence -> InstructionSequence))
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (append (statements seq) (statements body-seq))))

(: parallel-instruction-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence (append (statements seq1) (statements seq2))))

