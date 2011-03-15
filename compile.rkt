#lang typed/racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         "find-toplevel-variables.rkt"
         "sets.rkt"
         racket/list)

(provide (rename-out [-compile compile])
         compile-procedure-call
         append-instruction-sequences
         current-defined-name
         adjust-target-depth)


(: current-defined-name (Parameterof (U Symbol False)))
(define current-defined-name (make-parameter #f))


;(provide compile-top)

(: -compile (ExpressionCore Target Linkage -> (Listof Statement)))
(define (-compile exp target linkage)
  (statements
   (let ([end (make-label 'end)])
     (append-instruction-sequences
      (compile (make-Top (make-Prefix (find-toplevel-variables exp))
                         exp)
               (list)
               target 
               linkage)))))




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
     (compile-application exp cenv target linkage)]
    [(Let1? exp)
     (compile-let1 exp cenv target linkage)]
    [(Let? exp)
     (compile-let exp cenv target linkage)]
    [(LetRec? exp)
     (compile-letrec exp cenv target linkage)]))



(: compile-top (Top CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-top top cenv target linkage)
  (let*: ([cenv : CompileTimeEnvironment (extend-lexical-environment cenv (Top-prefix top))]
          [names : (Listof (U Symbol False)) (Prefix-names (Top-prefix top))])
         (append-instruction-sequences
          (make-instruction-sequence 
           `(,(make-PerformStatement (make-ExtendEnvironment/Prefix! names))))
          (compile (Top-code top) cenv target linkage))))



;; Add linkage for expressions.
(: end-with-linkage (Linkage CompileTimeEnvironment InstructionSequence ->
                             InstructionSequence))
(define (end-with-linkage linkage cenv instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-linkage cenv linkage)))


(: end-with-compiled-application-linkage (Linkage CompileTimeEnvironment InstructionSequence ->
                                                  InstructionSequence))
;; Add linkage for applications; we need to specialize this to preserve tail calls.
(define (end-with-compiled-application-linkage linkage cenv instruction-sequence)
  (append-instruction-sequences instruction-sequence
                                (compile-application-linkage cenv linkage)))



(: compile-linkage (CompileTimeEnvironment Linkage -> InstructionSequence))
(define (compile-linkage cenv linkage)
  (cond
    [(eq? linkage 'return)
     (make-instruction-sequence `(,(make-AssignPrimOpStatement 'proc
                                                               (make-GetControlStackLabel))
                                  ,(make-PopEnvironment 
                                    (lexical-environment-pop-depth cenv linkage)
                                    0)
                                  ,(make-PopControlFrame)
                                  ,(make-GotoStatement (make-Reg 'proc))))]
    [(eq? linkage 'next)
     empty-instruction-sequence]
    [(symbol? linkage)
     (make-instruction-sequence `(,(make-GotoStatement (make-Label linkage))))]))


(: compile-application-linkage (CompileTimeEnvironment Linkage -> InstructionSequence))
;; Like compile-linkage, but the special case for 'return linkage already assumes
;; the stack has been appropriately popped.
(define (compile-application-linkage cenv linkage)
  (cond
    [(eq? linkage 'return)
     (make-instruction-sequence `(,(make-AssignPrimOpStatement 'proc
                                                               (make-GetControlStackLabel))
                                  ,(make-PopControlFrame)
                                  ,(make-GotoStatement (make-Reg 'proc))))]
    [(eq? linkage 'next)
     (make-instruction-sequence `(,(make-PopEnvironment (lexical-environment-pop-depth cenv linkage)
                                                        0)))]
    [(symbol? linkage)
     (make-instruction-sequence `(,(make-PopEnvironment (lexical-environment-pop-depth cenv linkage)
                                                        0)
                                  ,(make-GotoStatement (make-Label linkage))))]))




(: lexical-environment-pop-depth (CompileTimeEnvironment Linkage -> Natural))
;; Computes how much of the environment we need to pop.
(define (lexical-environment-pop-depth cenv linkage)
  (length cenv))




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
                          `(,(make-AssignImmediateStatement 
                              target
                              (make-EnvLexicalReference (LocalAddress-depth lexical-pos)
                                                        (LocalAddress-unbox? lexical-pos))))))]
      [(PrefixAddress? lexical-pos)
       (end-with-linkage linkage
                         cenv
                         (make-instruction-sequence
                          `(,(make-PerformStatement (make-CheckToplevelBound!
                                                     (PrefixAddress-depth lexical-pos)
                                                     (PrefixAddress-pos lexical-pos)
                                                     (PrefixAddress-name lexical-pos)))
                            ,(make-AssignImmediateStatement 
                              target
                              (make-EnvPrefixReference
                               (PrefixAddress-depth lexical-pos)
                               (PrefixAddress-pos lexical-pos))))))])))


(: compile-definition (Def CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-definition exp cenv target linkage)
  (let* ([var (Def-variable exp)]
         [lexical-pos (find-variable var cenv)])
    (cond
      [(LocalAddress? lexical-pos)
       (error 'compile-definition "Defintion not at toplevel")]
      [(PrefixAddress? lexical-pos)
       (let ([get-value-code
              (parameterize ([current-defined-name var])
                (compile (Def-value exp) cenv (make-EnvPrefixReference 
                                               (PrefixAddress-depth lexical-pos)
                                               (PrefixAddress-pos lexical-pos))
                         'next))])
         (end-with-linkage
          linkage
          cenv
          (append-instruction-sequences
           get-value-code
           (make-instruction-sequence `(,(make-AssignImmediateStatement target (make-Const 'ok)))))))])))


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
                                        `(,(make-TestAndBranchStatement 'false? 
                                                                        'val
                                                                        f-branch)
                                          ))
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
          [lexical-references : (Listof EnvReference)
                              (collect-lexical-references lexical-addresses)])
         (append-instruction-sequences
          (end-with-linkage 
           lambda-linkage
           cenv
           (make-instruction-sequence 
            `(,(make-AssignPrimOpStatement target
                                           (make-MakeCompiledProcedure proc-entry
                                                                       (length (Lam-parameters exp))
                                                                       lexical-references
                                                                       (current-defined-name))))))
          (compile-lambda-body exp cenv
                               lexical-references
                               free-vars
                               proc-entry)
          after-lambda)))






(: compile-lambda-body (Lam CompileTimeEnvironment 
                            (Listof EnvReference)
                            (Listof Symbol)
                            Linkage 
                            -> 
                            InstructionSequence))
;; Compiles the body of the lambda in the appropriate environment.
(define (compile-lambda-body exp cenv lexical-references free-variables proc-entry)
  (let*: ([formals : (Listof Symbol) (Lam-parameters exp)]
          [extended-cenv : CompileTimeEnvironment 
                         (extend-lexical-environment/names
                          '() 
                          formals)]
          [extended-cenv : CompileTimeEnvironment 
                         (lexical-references->compile-time-environment 
                          lexical-references cenv extended-cenv
                          free-variables)])
         (append-instruction-sequences
          (make-instruction-sequence 
           `(,proc-entry
             ,(make-PerformStatement (make-InstallClosureValues!))))
          (compile (Lam-body exp) extended-cenv 'val 'return))))



#;(: compile-letrec (Letrec CompileTimeEnvironment Target Linkage -> InstructionSequence))
#;(define (compile-letrec exp cenv target linkage)
  (let* ([after-let (make-label 'afterLet)]
         [let-linkage (if (eq? linkage 'next)
                          after-let
                          linkage)]
         [extended-cenv : CompileTimeEnvironment
                        (extend-lexical-environment/names
                         '()
                         (reverse (Letrec-names exp)))]
         [lam-codes : (Listof InstructionSequence)
                    (let: ([n : Natural (length (Letrec-procs exp))])
                    (map (lambda: ([lam : Lam]
                                   [target : Target])
                                  (compile-lambda lam extended-cenv target 'next))
                         (Letrec-procs exp)
                         (build-list (length (Letrec-procs exp))
                                     (lambda: ([i : Natural])
                                              (make-EnvLexicalReference (- n 1 i))))))]
         [body-code : InstructionSequence 
                    (compile (Letrec-body exp) extended-cenv target let-linkage)]
   (append-instruction-sequences
    (end-with-linkage let-linkage cenv
     (make-instruction-sequence `(;; create space for the lambdas
                                  ,(make-PushEnvironment n)  
                                  ;; install each one of them in place
                                  (apply append-instruction-sequences lam-codes)
                                  ;; mutate each of the lambda's shells so they're correct
                                  
                                  ;; evaluate the body
                                  body-code
                                  ;; pop the temporary space
                                  )))
    after-let))))

         


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
                                 (make-EnvLexicalReference 
                                  (ensure-natural (sub1 (length (App-operands exp))))
                                  #f))
                             'next)]
         [operand-codes (map (lambda: ([operand : Expression]
                                       [target : Target])
                                      (compile operand extended-cenv target 'next))
                             (App-operands exp)
                             (build-list (length (App-operands exp))
                                         (lambda: ([i : Natural])
                                                  (if (< i (sub1 (length (App-operands exp))))
                                                      (make-EnvLexicalReference i #f)
                                                      'val))))])
    
    (append-instruction-sequences
     (make-instruction-sequence `(,(make-PushEnvironment (length (App-operands exp)) #f)))
     proc-code
     (juggle-operands operand-codes)
     (compile-procedure-call cenv extended-cenv 
                             (length (App-operands exp)) 
                             target linkage))))



(: juggle-operands ((Listof InstructionSequence) -> InstructionSequence))
;; Installs the operators.  At the end of this,
;; the procedure lives in 'proc, and the operands on the environment stack.
(define (juggle-operands operand-codes)
  (let: loop : InstructionSequence ([ops : (Listof InstructionSequence) operand-codes])
        (cond
          ;; If there are no operands, no need to juggle.
          [(null? ops)
           (make-instruction-sequence empty)]
          [(null? (rest ops))
           (let: ([n : Natural (ensure-natural (sub1 (length operand-codes)))])
                 ;; The last operand needs to be handled specially: it currently lives in 
                 ;; val.  We move the procedure at env[n] over to proc, and move the
                 ;; last operand at 'val into env[n].
                 (append-instruction-sequences 
                  (car ops)
                  (make-instruction-sequence 
                   `(,(make-AssignImmediateStatement 'proc 
                                                     (make-EnvLexicalReference n #f))
                     ,(make-AssignImmediateStatement (make-EnvLexicalReference n #f)
                                                     (make-Reg 'val))))))]
          [else
           ;; Otherwise, add instructions to juggle the operator and operands in the stack.
           (append-instruction-sequences (car ops)
                                         (loop (rest ops)))])))



(: compile-procedure-call (CompileTimeEnvironment CompileTimeEnvironment 
                                                  Natural Target Linkage 
                                                  ->
                                                  InstructionSequence))
;; Assumes the procedure value has been loaded into the proc register.
;; n is the number of arguments passed in.
;; cenv is the compile-time enviroment before arguments have been shifted in.
;; extended-cenv is the compile-time environment after arguments have been shifted in.
(define (compile-procedure-call cenv extended-cenv n target linkage)
  (let ([primitive-branch (make-label 'primitiveBranch)]
        [compiled-branch (make-label 'compiledBranch)]
        [after-call (make-label 'afterCall)])
    (let ([compiled-linkage (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence 
        `(,(make-TestAndBranchStatement 'primitive-procedure?
                                        'proc
                                        primitive-branch)))
       
       compiled-branch
       (make-instruction-sequence
        `(,(make-PerformStatement (make-CheckClosureArity! n))))
       (end-with-compiled-application-linkage 
        compiled-linkage
        extended-cenv
        (compile-proc-appl extended-cenv n target compiled-linkage))
       
       primitive-branch
       (end-with-linkage
        linkage
        cenv
        (make-instruction-sequence 
         `(,(make-AssignPrimOpStatement 
             ;; Optimization: we put the result directly in the registers, or in
             ;; the appropriate spot on the stack.  This takes into account the popenviroment
             ;; that happens right afterwards.
             (adjust-target-depth target n)                     
             (make-ApplyPrimitiveProcedure n after-call))
           ,(make-PopEnvironment n 0))))

       after-call))))



(: compile-proc-appl (CompileTimeEnvironment Natural Target Linkage -> InstructionSequence))
;; Three fundamental cases for general compiled-procedure application.
;;    1.  Non-tail calls that write to val
;;    2.  Calls in argument position that write to the environment
;;    3.  Tail calls.
;; The Other cases should be excluded.
(define (compile-proc-appl cenv n target linkage)
  (cond [(and (eq? target 'val)
              (not (eq? linkage 'return)))
         ;; This case happens for a function call that isn't in
         ;; tail position.
         (make-instruction-sequence 
          `(,(make-PushControlFrame linkage)
            ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
            ,(make-GotoStatement (make-Reg 'val))))]
        
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         ;; This case happens for evaluating arguments, since the
         ;; arguments are being installed into the scratch space.
         (let ([proc-return (make-label 'procReturn)])
           (make-instruction-sequence
            `(,(make-PushControlFrame proc-return)
              ,(make-AssignPrimOpStatement 'val (make-GetCompiledProcedureEntry))
              ,(make-GotoStatement (make-Reg 'val))
              ,proc-return
              ,(make-AssignImmediateStatement target (make-Reg 'val))
              ,(make-GotoStatement (make-Label linkage)))))]
        
        [(and (eq? target 'val)
              (eq? linkage 'return))
         ;; This case happens when we're in tail position.
         ;; We clean up the stack right before the jump, and do not add
         ;; to the control stack.
         (make-instruction-sequence
          `(,(make-AssignPrimOpStatement 'val 
                                         (make-GetCompiledProcedureEntry))
            ,(make-PopEnvironment (ensure-natural (- (lexical-environment-pop-depth cenv linkage)
                                                     n))
                                  n)
            ,(make-GotoStatement (make-Reg 'val))))]
        
        [(and (not (eq? target 'val))
              (eq? linkage 'return))
         ;; This case should be impossible: return linkage should only
         ;; occur when we're in tail position, and we're in tail position
         ;; only when the target is the val register.
         (error 'compile "return linkage, target not val: ~s" target)]))


(: compile-let1 (Let1 CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-let1 exp cenv target linkage)
  (let*: ([rhs-code : InstructionSequence 
                    (parameterize ([current-defined-name (Let1-name exp)])
                      (compile (Let1-rhs exp)
                               (extend-lexical-environment/placeholders cenv 1)
                               (make-EnvLexicalReference 0 #f)
                               'next))]
          [after-let1 : Symbol (make-label 'afterLetOne)]
          [after-body-code : Symbol (make-label 'afterLetBody)]
          [extended-cenv : CompileTimeEnvironment
                         (extend-lexical-environment/names cenv (list (Let1-name exp)))]
          [let-linkage : Linkage
                       (cond
                         [(eq? linkage 'next)
                          'next]
                         [(eq? linkage 'return)
                          'return]
                         [(symbol? linkage)
                          after-body-code])]
          [body-target : Target (adjust-target-depth target 1)]
          [body-code : InstructionSequence
                     (compile (Let1-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences
           (make-instruction-sequence `(,(make-PushEnvironment 1 #f)))
           rhs-code
           body-code
           after-body-code
           (make-instruction-sequence `(,(make-PopEnvironment 1 0)))
           after-let1))))



(: compile-let (Let CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-let exp cenv target linkage)
  (let*: ([n : Natural (length (Let-rhss exp))]
          [rhs-codes : (Listof InstructionSequence)
                     (map (lambda: ([rhs : ExpressionCore]
                                    [i : Natural]
                                    [name : Symbol])
                                   (parameterize ([current-defined-name name])
                                     (compile rhs
                                              (extend-lexical-environment/placeholders cenv n)
                                              (make-EnvLexicalReference i #f)
                                              'next)))
                          (Let-rhss exp)
                          (build-list n (lambda: ([i : Natural]) i))
                          (Let-names exp))]
          [after-let : Symbol (make-label 'afterLet)]
          [after-body-code : Symbol (make-label 'afterLetBody)]
          [extended-cenv : CompileTimeEnvironment
                         (extend-lexical-environment/names cenv (Let-names exp))]
          [let-linkage : Linkage
                       (cond
                         [(eq? linkage 'next)
                          'next]
                         [(eq? linkage 'return)
                          'return]
                         [(symbol? linkage)
                          after-body-code])]
          [body-target : Target (adjust-target-depth target n)]
          [body-code : InstructionSequence
                     (compile (Let-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences 
           (make-instruction-sequence `(,(make-PushEnvironment n #f)))
           (apply append-instruction-sequences rhs-codes)
           body-code
           after-body-code
           (make-instruction-sequence `(,(make-PopEnvironment n 0)))
           after-let))))

(: compile-letrec (LetRec CompileTimeEnvironment Target Linkage -> InstructionSequence))
(define (compile-letrec exp cenv target linkage)
  (let*: ([n : Natural (length (LetRec-rhss exp))]
          [rhs-codes : (Listof InstructionSequence)
                     (map (lambda: ([rhs : ExpressionCore]
                                    [i : Natural]
                                    [name : Symbol])
                                   (parameterize ([current-defined-name name])
                                     (compile rhs
                                              (extend-lexical-environment/boxed-names cenv 
                                                                                      (LetRec-names exp))
                                              (make-EnvLexicalReference i #t)
                                              'next)))
                          (LetRec-rhss exp)
                          (build-list n (lambda: ([i : Natural]) i))
                          (LetRec-names exp))]
          [after-letrec : Symbol (make-label 'afterLetRec)]
          [after-body-code : Symbol (make-label 'afterLetBody)]
          [extended-cenv : CompileTimeEnvironment
                         (extend-lexical-environment/boxed-names cenv (LetRec-names exp))]
          [let-linkage : Linkage
                       (cond
                         [(eq? linkage 'next)
                          'next]
                         [(eq? linkage 'return)
                          'return]
                         [(symbol? linkage)
                          after-body-code])]
          [body-target : Target (adjust-target-depth target n)]
          [body-code : InstructionSequence
                     (compile (LetRec-body exp) extended-cenv body-target let-linkage)])
         (end-with-linkage 
          linkage
          extended-cenv
          (append-instruction-sequences
           (make-instruction-sequence `(,(make-PushEnvironment n #t)))
           (apply append-instruction-sequences rhs-codes)
           body-code
           after-body-code
           (make-instruction-sequence `(,(make-PopEnvironment n 0)))
           after-letrec))))


(: adjust-target-depth (Target Natural -> Target))
(define (adjust-target-depth target n)
  (cond
    [(eq? target 'val)
     target]
    [(eq? target 'proc)
     target]
    [(EnvLexicalReference? target)
     (make-EnvLexicalReference (+ n (EnvLexicalReference-depth target))
                               (EnvLexicalReference-unbox? target))]
    [(EnvPrefixReference? target)
     (make-EnvPrefixReference (+ n (EnvPrefixReference-depth target))
                              (EnvPrefixReference-pos target))]
    [(PrimitivesReference? target)
     target]))



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

(: ensure-natural (Integer -> Natural))
(define (ensure-natural n)
  (if (>= n 0)
      n
      (error 'ensure-natural "Not a natural: ~s\n" n)))


