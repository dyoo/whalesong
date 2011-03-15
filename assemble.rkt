#lang typed/racket/base
(require "il-structs.rkt"
         "helpers.rkt"
         racket/string
         racket/list)

(provide assemble/write-invoke
         fracture
         assemble-basic-block
         assemble-statement)


(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
;; Writes out the JavaScript code that represents the anonymous invocation expression.
(define (assemble/write-invoke stmts op)
  (let ([basic-blocks (fracture stmts)])
    (fprintf op "(function(success, fail, params) {\n")
    (fprintf op "var param;\n")
    (for-each (lambda: ([basic-block : BasicBlock])
                (displayln (assemble-basic-block basic-block) op)
                (newline op))
              basic-blocks)
    (fprintf op "MACHINE.params.currentErrorHandler = function(e) { fail(e); };\n")
    (fprintf op #<<EOF
for (param in params) {
    if (params.hasOwnProperty(param)) {
        MACHINE.params[param] = params[param];
    }
}
EOF
             )
    (fprintf op "trampoline(~a, function() {success(MACHINE.val)}, fail); })"
             (BasicBlock-name (first basic-blocks)))))




;; fracture: (listof stmt) -> (listof basic-block)
(: fracture ((Listof Statement) -> (Listof BasicBlock)))
(define (fracture stmts)
  (let* ([first-block-label (if (and (not (empty? stmts))
                                     (symbol? (first stmts)))
                                (first stmts)
                                (make-label 'start))]
         [stmts (if (and (not (empty? stmts))
                                     (symbol? (first stmts)))
                    (rest stmts)
                    stmts)]
         [jump-targets 
          (cons first-block-label (collect-general-jump-targets stmts))])
    (let: loop : (Listof BasicBlock)
          ([name : Symbol first-block-label]
           [acc : (Listof UnlabeledStatement) '()]
           [basic-blocks  : (Listof BasicBlock) '()]
           [stmts : (Listof Statement) stmts]
           [last-stmt-goto? : Boolean #f])
      (cond
        [(null? stmts)
         (reverse (cons (make-BasicBlock name (reverse acc))
                        basic-blocks))]
        [(symbol? (car stmts))
         (cond
           [(member (car stmts) jump-targets)
            (loop (car stmts)
                  '()
                  (cons (make-BasicBlock name  
                                          (if last-stmt-goto? 
                                              (reverse acc)
                                              (reverse (append `(,(make-GotoStatement (make-Label (car stmts))))
                                                               acc))))
                        basic-blocks)
                  (cdr stmts)
                  last-stmt-goto?)]
           [else
            (loop name
                  acc
                  basic-blocks
                  (cdr stmts)
                  last-stmt-goto?)])]
        [else
         (loop name
               (cons (car stmts) acc)
               basic-blocks
               (cdr stmts)
               (GotoStatement? (car stmts)))]))))






;; collect-general-jump-targets: (listof stmt) -> (listof label)
;; collects all the labels that are potential targets for GOTOs or branches.
(: collect-general-jump-targets ((Listof Statement) -> (Listof Symbol)))
(define (collect-general-jump-targets stmts)
  (: collect-input (OpArg -> (Listof Symbol)))
  (define (collect-input an-input)
    (cond
      [(Reg? an-input)
       empty]
      [(Const? an-input)
       empty]
      [(Label? an-input)
       (list (Label-name an-input))]
      [(EnvLexicalReference? an-input)
       empty]
      [(EnvPrefixReference? an-input)
       empty]
      [(EnvWholePrefixReference? an-input)
       empty]))

  (: collect-location ((U Reg Label) -> (Listof Symbol)))
  (define (collect-location a-location)
    (cond
      [(Reg? a-location)
       empty]
      [(Label? a-location)
       (list (Label-name a-location))]))
  
  (: collect-primitive-operator (PrimitiveOperator -> (Listof Symbol)))
  (define (collect-primitive-operator op)
    (cond
      [(GetCompiledProcedureEntry? op)
       empty]
      [(MakeCompiledProcedure? op)
       (list (MakeCompiledProcedure-label op))]
      [(ApplyPrimitiveProcedure? op)
       (list (ApplyPrimitiveProcedure-label op))]
      [(GetControlStackLabel? op)
       empty]
      [(CaptureEnvironment? op)
       empty]
      [(CaptureControl? op)
       empty]))

  (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
  (define (collect-primitive-command op)
    (cond
      [(CheckToplevelBound!? op)
       empty]
      [(CheckClosureArity!? op)
       empty]
      [(ExtendEnvironment/Prefix!? op)
       empty]
      [(InstallClosureValues!? op)
       empty]
      [(RestoreEnvironment!? op)
       empty]
      [(RestoreControl!? op)
       empty]))
  
  (unique/eq?
   (let: loop : (Listof Symbol) ([stmts : (Listof Statement) stmts])
     (cond [(empty? stmts)
            empty]
           [else
            (let ([stmt (first stmts)])
              (append (cond
                        [(symbol? stmt)
                         empty]
                        [(AssignImmediateStatement? stmt)
                         (let: ([v : OpArg (AssignImmediateStatement-value stmt)])
                           (cond 
                             [(Reg? v)
                              empty]
                             [(Label? v)
                              (list (Label-name v))]
                             [(Const? v)
                              empty]
                             [(EnvLexicalReference? v)
                              empty]
                             [(EnvPrefixReference? v)
                              empty] 
                             [(EnvWholePrefixReference? v)
                              empty]))]
                        [(AssignPrimOpStatement? stmt)
                         (collect-primitive-operator (AssignPrimOpStatement-op stmt))]
                        [(PerformStatement? stmt)
                         (collect-primitive-command (PerformStatement-op stmt))]
                        [(TestAndBranchStatement? stmt)
                         (list (TestAndBranchStatement-label stmt))]
                        [(GotoStatement? stmt)
                         (collect-location (GotoStatement-target stmt))]
                        [(PushEnvironment? stmt)
                         empty]
                        [(PopEnvironment? stmt)
                         empty]
                        [(PushControlFrame? stmt)
                         (list (PushControlFrame-label stmt))]
                        [(PopControlFrame? stmt)
                         empty])
                      (loop (rest stmts))))]))))




;; assemble-basic-block: basic-block -> string
(: assemble-basic-block (BasicBlock -> String))
(define (assemble-basic-block a-basic-block)
  (format "var ~a=function(){\nif(--MACHINE.callsBeforeTrampoline < 0) { throw ~a; }\n~a};"
          (BasicBlock-name a-basic-block)
          (BasicBlock-name a-basic-block)
          (string-join (map assemble-statement (BasicBlock-stmts a-basic-block))
                       "\n")))

(: assemble-oparg (OpArg -> String))
(define (assemble-oparg v)
  (cond 
    [(Reg? v)
     (assemble-reg v)]
    [(Label? v)
     (assemble-label v)]
    [(Const? v)
     (assemble-const v)]
    [(EnvLexicalReference? v)
     (assemble-lexical-reference v)]
    [(EnvPrefixReference? v)
     (assemble-prefix-reference v)]
    [(EnvWholePrefixReference? v)
     (assemble-whole-prefix-reference v)]))


(: assemble-statement (UnlabeledStatement -> String))
;; Generates the code to assemble a statement.
(define (assemble-statement stmt)
  (cond
    [(AssignImmediateStatement? stmt)
     (let ([t (assemble-target (AssignImmediateStatement-target stmt))]
           [v (AssignImmediateStatement-value stmt)])
       (format "~a = ~a;" t (assemble-oparg v)))]
    
    [(AssignPrimOpStatement? stmt)
     (format "~a=~a;" 
             (assemble-target (AssignPrimOpStatement-target stmt))
             (assemble-op-expression (AssignPrimOpStatement-op stmt)))]
    
    [(PerformStatement? stmt)
     (assemble-op-statement (PerformStatement-op stmt))]
    
    [(TestAndBranchStatement? stmt)
     (let*: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)])
            (cond
              [(eq? test 'false?)
               (format "if (! ~a) { return ~a(); }"
                       (assemble-reg (make-Reg (TestAndBranchStatement-register stmt)))
                       (assemble-label (make-Label (TestAndBranchStatement-label stmt))))]
              [(eq? test 'primitive-procedure?)
               (format "if (typeof(~a) === 'function') { return ~a(); };"
                       (assemble-reg (make-Reg (TestAndBranchStatement-register stmt)))
                       (assemble-label (make-Label (TestAndBranchStatement-label stmt))))]))]

    [(GotoStatement? stmt)
     (format "return ~a();" (assemble-location (GotoStatement-target stmt)))]
    [(PushControlFrame? stmt)
     (format "MACHINE.control.push(new Frame(~a, MACHINE.proc));" (PushControlFrame-label stmt))]
    [(PopControlFrame? stmt)
     "MACHINE.control.pop();"]
    [(PushEnvironment? stmt)
     (format "MACHINE.env.push(~a);" (string-join
                                      (build-list (PushEnvironment-n stmt) 
                                                  (lambda: ([i : Natural])
                                                           (if (PushEnvironment-unbox? stmt)
                                                               "[undefined]"
                                                               "undefined")))
                                      ", "))]
    [(PopEnvironment? stmt)
     (format "MACHINE.env.splice(MACHINE.env.length-(~a),~a);"
             (+ (PopEnvironment-skip stmt)
                (PopEnvironment-n stmt))
             (PopEnvironment-n stmt))]))



(: assemble-target (Target -> String))
(define (assemble-target target)
  (cond
    [(eq? target 'proc)
     "MACHINE.proc"]
    [(eq? target 'val)
     "MACHINE.val"]
    [(EnvLexicalReference? target)
     (assemble-lexical-reference target)]
    [(EnvPrefixReference? target)
     (assemble-prefix-reference target)]))




;; fixme: use js->string
(: assemble-const (Const -> String))
(define (assemble-const stmt)
  (let: loop : String ([val : Any (Const-const stmt)])
        (cond [(symbol? val)
               (format "~s" (symbol->string val))]
              [(pair? val)
               (format "[~a, ~a]" 
                       (loop (car val))
                       (loop (cdr val)))]
              [(boolean? val)
               (if val "true" "false")]
              [(empty? val)
               (format "Primitives.null")]
              [else
               (format "~s" val)])))


(: assemble-lexical-reference (EnvLexicalReference -> String))
(define (assemble-lexical-reference a-lex-ref)
  (if (EnvLexicalReference-unbox? a-lex-ref)
      (format "MACHINE.env[MACHINE.env.length - 1 - ~a][0]"
              (EnvLexicalReference-depth a-lex-ref))
      (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
              (EnvLexicalReference-depth a-lex-ref))))

(: assemble-prefix-reference (EnvPrefixReference -> String))
(define (assemble-prefix-reference a-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a][~a]"
          (EnvPrefixReference-depth a-ref)
          (EnvPrefixReference-pos a-ref)))

(: assemble-whole-prefix-reference (EnvWholePrefixReference -> String))
(define (assemble-whole-prefix-reference a-prefix-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          (EnvWholePrefixReference-depth a-prefix-ref)))


(: assemble-env-reference/closure-capture (EnvReference -> String))
;; When we're capturing the values for a closure, we need to not unbox
;; lexical references: they must remain boxes.
(define (assemble-env-reference/closure-capture ref)
  (cond
    [(EnvLexicalReference? ref)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
             (EnvLexicalReference-depth ref))]
    [(EnvWholePrefixReference? ref)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
             (EnvWholePrefixReference-depth ref))]))

(: assemble-display-name ((U Symbol False) -> String))
(define (assemble-display-name symbol-or-string)
  (if (symbol? symbol-or-string)
      (format "~s" (symbol->string symbol-or-string))
      "false"))

(: assemble-op-expression (PrimitiveOperator -> String))
(define (assemble-op-expression op)
  (cond
    [(GetCompiledProcedureEntry? op)
     "MACHINE.proc.label"]
    
    [(MakeCompiledProcedure? op)
     (format "new Closure(~a, ~a, [~a], ~a)"
             (MakeCompiledProcedure-label op)
             (MakeCompiledProcedure-arity op)
             (string-join (map assemble-env-reference/closure-capture 
                               ;; The closure values are in reverse order
                               ;; to make it easier to push, in bulk, into
                               ;; the environment (which is also in reversed order)
                               ;; during install-closure-values.
                               (reverse (MakeCompiledProcedure-closed-vals op)))
                          ", ")
             (assemble-display-name (MakeCompiledProcedure-display-name op)))]
    
    [(ApplyPrimitiveProcedure? op)
     (format "MACHINE.proc(~a, ~a)"
             (ApplyPrimitiveProcedure-arity op)
             (ApplyPrimitiveProcedure-label op))]

    [(GetControlStackLabel? op)
     (format "MACHINE.control[MACHINE.control.length-1].label")]
    [(CaptureEnvironment? op)
     (format "MACHINE.env.slice(0, MACHINE.env.length - ~a)"
             (CaptureEnvironment-skip op))]
    [(CaptureControl? op)
     (format "MACHINE.control.slice(0, MACHINE.control.length - ~a)"
             (CaptureControl-skip op))]))


(: assemble-op-statement (PrimitiveCommand -> String))
(define (assemble-op-statement op)  
  (cond 
    
    [(CheckToplevelBound!? op)
     (format "if (MACHINE.env[MACHINE.env.length - 1 - ~a][~a] === undefined) { throw new Error(\"Not bound: \" + ~s); }"
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op)
             (symbol->string (CheckToplevelBound!-name op)))]
    
    [(CheckClosureArity!? op)
     (format "if (! (MACHINE.proc instanceof Closure && MACHINE.proc.arity === ~a)) { if (! (MACHINE.proc instanceof Closure)) { throw new Error(\"not a closure\"); } else { throw new Error(\"arity failure\"); } }"
             (CheckClosureArity!-arity op)
             )]
    
    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof (U Symbol False)) (ExtendEnvironment/Prefix!-names op)])
           (format "MACHINE.env.push([~a]);"
                   (string-join (map (lambda: ([n : (U Symbol False)])
                                              (if (symbol? n)
                                                  (format "MACHINE.params.currentNamespace[~s] || Primitives[~s]"
                                                          (symbol->string n) 
                                                          (symbol->string n))
                                                  "false"))
                                     names)
                                ",")))]
    
    [(InstallClosureValues!? op)
     "MACHINE.env.splice.apply(MACHINE.env, [MACHINE.env.length, 0].concat(MACHINE.proc.closedVals));"]
    [(RestoreEnvironment!? op)
     "MACHINE.env = MACHINE.env[MACHINE.env.length - 2].slice(0);"]
    [(RestoreControl!? op)
     "MACHINE.control = MACHINE.env[MACHINE.env.length - 1].slice(0);"]))


(: assemble-input (OpArg -> String))
(define (assemble-input an-input)
  (cond
    [(Reg? an-input)
     (assemble-reg an-input)]
    [(Const? an-input)
     (assemble-const an-input)]
    [(Label? an-input)
     (assemble-label an-input)]
    [(EnvLexicalReference? an-input)
     (assemble-lexical-reference an-input)]
    [(EnvPrefixReference? an-input)
     (assemble-prefix-reference an-input)]
    [(EnvWholePrefixReference? an-input)
     (assemble-whole-prefix-reference an-input)]))
    
(: assemble-location ((U Reg Label) -> String))
(define (assemble-location a-location)
  (cond
    [(Reg? a-location)
     (assemble-reg a-location)]
    [(Label? a-location)
     (assemble-label a-location)]))

(: assemble-reg (Reg -> String))
(define (assemble-reg a-reg)
  (string-append "MACHINE." (symbol->string (Reg-name a-reg))))

(: assemble-label (Label -> String))
(define (assemble-label a-label)
  (symbol->string (Label-name a-label)))