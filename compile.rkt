#lang typed/racket/base

(require "typed-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         racket/list)

(provide compile)

;; SICP, Chapter 5.5

;; registers: env, argl, proc, val, cont
;; as well as the stack.
(define all-regs '(env argl proc val cont))





;; compile: expression target linkage -> instruction-sequence
(: compile (Expression CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile exp cenv target linkage)
  (cond
    [(Constant? exp)
     (compile-self-evaluating exp cenv target linkage)]
    [(Quote? exp)
     (compile-quoted exp cenv target linkage)]
    [(Var? exp)
     (compile-variable exp cenv target linkage)]
    [(Assign? exp)
     (compile-assignment exp cenv target linkage)]
    [(Def? exp)
     (compile-definition exp cenv target linkage)]
    [(Branch? exp)
     (compile-if exp cenv target linkage)]
    [(Lam? exp)
     (compile-lambda exp cenv target linkage)]
    [(Seq? exp)
     (compile-sequence (Seq-actions exp)
                       cenv
                       target
                       linkage)]
    [(App? exp)
     (compile-application exp cenv target linkage)]))



(: compile-linkage (Linkage -> InstructionSequence))
(define (compile-linkage linkage)
  (cond
    [(eq? linkage 'return)
     (make-instruction-sequence '(cont) '() `(,(make-GotoStatement (make-Reg 'cont))))]
    [(eq? linkage 'next)
     empty-instruction-sequence]
    [else
     (make-instruction-sequence '() '()
                                `(,(make-GotoStatement (make-Label linkage))))]))

(: end-with-linkage (Linkage InstructionSequence -> InstructionSequence))
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(cont)
              instruction-sequence
              (compile-linkage linkage)))

(: compile-self-evaluating (Constant CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-self-evaluating exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `(,(make-AssignImmediateStatement target (make-Const exp))))))

(: compile-quoted (Quote CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-quoted exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `(,(make-AssignImmediateStatement target (make-Const (Quote-text exp)))))))

(: compile-variable (Var CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-variable exp cenv target linkage)
  (let ([lexical-pos (find-variable (Var-id exp) cenv)])
    (cond
      [(eq? lexical-pos 'not-found)
       (end-with-linkage linkage
                         (make-instruction-sequence
                          '(env)
                          (list target)
                          ;; Slight modification: explicitly testing for
                          ;; global variable binding before lookup.
                          `(,(make-PerformStatement 'check-bound-global!
                                                    (list (make-Const (Var-id exp))
                                                          (make-Reg 'env)))
                            ,(make-AssignPrimOpStatement target
                                    'lookup-variable-value
                                    (list (make-Const (Var-id exp))
                                          (make-Reg 'env))))))]
      [else
       (end-with-linkage linkage
                         (make-instruction-sequence
                          '(env)
                          (list target)
                          `(,(make-AssignPrimOpStatement target
                                                         'lexical-address-lookup
                                                         (list (make-Const (first lexical-pos))
                                                               (make-Const (second lexical-pos))
                                                               (make-Reg 'env))))))])))


(: compile-assignment (Assign CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-assignment exp cenv target linkage)
  (let* ([var (Assign-variable exp)]
         [get-value-code
          (compile (Assign-value exp) cenv 'val 'next)]
         [lexical-address
          (find-variable var cenv)])
    (cond
      [(eq? lexical-address 'not-found)
       (end-with-linkage
        linkage
        (preserving '(env)
                    get-value-code
                    (make-instruction-sequence
                     '(env val)
                     (list target)
                     `(,(make-PerformStatement 'set-variable-value!
                                               (list (make-Const var)
                                                     (make-Reg 'val)
                                                     (make-Reg 'env)))
                       ,(make-AssignImmediateStatement target (make-Const 'ok))))))]
      [else
       (end-with-linkage
        linkage
        (preserving '(env)
                    get-value-code
                    (make-instruction-sequence
                     '(env val)
                     (list target)
                     `(,(make-PerformStatement 'lexical-address-set!
                                (list (make-Const (first lexical-address))
                                      (make-Const (second lexical-address))
                                      (make-Reg 'env)
                                      (make-Reg 'val)))
                       ,(make-AssignImmediateStatement target (make-Const 'ok))))))])))


;; FIXME: exercise 5.43
(: compile-definition (Def CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-definition exp cenv target linkage)
  (let ([var (Def-variable exp)]
        [get-value-code
         (compile (Def-value exp) cenv 'val 'next)])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `(,(make-PerformStatement 'define-variable!
                                 (list (make-Const var)
                                       (make-Reg 'val)
                                       (make-Reg 'env)))
         ,(make-AssignImmediateStatement target (make-Const 'ok))))))))


(: compile-if (Branch CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-if exp cenv target linkage)  
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
        (preserving '(env cont)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val)
                      '()
                      `(,(make-TestStatement 'false? 'val)
                        ,(make-BranchLabelStatement f-branch)))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))


(: compile-sequence ((Listof Expression) CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-sequence seq cenv target linkage) 
  (if (last-exp? seq)
      (compile (first-exp seq) cenv target linkage)
      (preserving '(env cont)
                  (compile (first-exp seq) cenv target 'next)
                  (compile-sequence (rest-exps seq) cenv target linkage))))


(: compile-lambda (Lam CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-lambda exp cenv target linkage) 
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
                           '(env)
                           (list target)
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

(: compile-lambda-body (Lam CompileTimeEnvironment Linkage -> InstructionSequence))
(define (compile-lambda-body exp cenv proc-entry)
  (let* ([formals (Lam-parameters exp)]
         [extended-cenv (extend-lexical-environment cenv formals)])
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        ,(make-AssignPrimOpStatement 'env 
                                     'compiled-procedure-env
                                     (list (make-Reg 'proc)))
        ,(make-AssignPrimOpStatement 'env
                'extend-environment
                (list (make-Reg 'argl)
                      (make-Reg 'env)))))
     (compile-sequence (Lam-body exp) extended-cenv 'val 'return))))

(: compile-application (App CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-application exp cenv target linkage) 
  (let ([proc-code (compile (App-operator exp) cenv 'proc 'next)]
        [operand-codes (map (lambda: ([operand : Expression])
                              (compile operand cenv 'val 'next))
                            (App-operands exp))])
    (preserving '(env cont)
                proc-code
                (preserving '(proc cont)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(: construct-arglist ((Listof InstructionSequence) -> InstructionSequence))
(define (construct-arglist operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (null? operand-codes)
        (make-instruction-sequence '()
                                   '(argl) 
                                   `(,(make-AssignImmediateStatement 'argl (make-Const '()))))
        (let ([code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           `(,(make-AssignPrimOpStatement 'argl 'list 
                                                                          (list (make-Reg 'val))))))])
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))
(: code-to-get-rest-args ((Listof InstructionSequence) -> InstructionSequence))
(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence 
                      '(val argl) 
                      '(argl)
                      `(,(make-AssignPrimOpStatement 'argl 
                                                     'cons 
                                                     (list (make-Reg 'val)
                                                           (make-Reg 'argl))))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(: compile-procedure-call (Target Linkage -> InstructionSequence))
(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-label 'primitiveBranch)]
        [compiled-branch (make-label 'compiledBranch)]
        [after-call (make-label 'afterCall)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `(,(make-TestStatement 'primitive-procedure? 'proc)
                                    ,(make-BranchLabelStatement primitive-branch)))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence 
                            '(proc argl)
                            (list target)
                            `(,(make-AssignPrimOpStatement target
                                                           'apply-primitive-procedure
                                                           (list (make-Reg 'proc)
                                                                 (make-Reg 'argl))))))))
       after-call))))

(: compile-proc-appl (Target Linkage -> InstructionSequence))
(define (compile-proc-appl target linkage)
  (cond [(and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc) 
          all-regs
          `(,(make-AssignImmediateStatement 'cont (make-Label linkage))
            ,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                         (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ([proc-return (make-label 'procReturn)])
           (make-instruction-sequence
            '(proc)
            all-regs
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
          '(proc cont)
          all-regs
          `(,(make-AssignPrimOpStatement 'val 'compiled-procedure-entry
                                        (list (make-Reg 'proc)))
            ,(make-GotoStatement (make-Reg 'val))))]
        [(and (not (eq? target 'val))
              (eq? linkage 'return))
         (error 'compile "return linkage, target not val: ~s" target)]))









(: needs-register? (InstructionSequence Symbol -> Boolean))
(define (needs-register? seq reg)
  (and (memq reg (registers-needed seq)) #t))

(: modifies-register? (InstructionSequence Symbol -> Boolean))
(define (modifies-register? seq reg)
  (and (memq reg (registers-modified seq)) #t))

(: preserving ((Listof Symbol) InstructionSequence InstructionSequence -> InstructionSequence))
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ([first-reg (car regs)])
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `(,(make-SaveStatement first-reg))
                                 (statements seq1)
                                 `(,(make-RestoreStatement first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))



(: append-instruction-sequences (InstructionSequence * -> InstructionSequence))
(define (append-instruction-sequences . seqs)
  (append-seq-list seqs))

(: append-2-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))

(: append-seq-list ((Listof InstructionSequence) -> InstructionSequence))
(define (append-seq-list seqs)
  (if (null? seqs)
      empty-instruction-sequence
      (append-2-sequences (car seqs)
                          (append-seq-list (cdr seqs)))))




(: tack-on-instruction-sequence (InstructionSequence InstructionSequence -> InstructionSequence))
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
                             (registers-modified seq)
                             (append (statements seq) (statements body-seq))))

(: parallel-instruction-sequences (InstructionSequence InstructionSequence -> InstructionSequence))
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

