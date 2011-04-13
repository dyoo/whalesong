#lang typed/racket/base
(require "il-structs.rkt"
         "lexical-structs.rkt"
         "helpers.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         racket/string
         racket/list)

(provide assemble/write-invoke
         fracture
         assemble-basic-block
         assemble-statement)


;; Parameter that controls the generation of a trace.
(define current-emit-debug-trace? (make-parameter #f))



(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
;; Writes out the JavaScript code that represents the anonymous invocation expression.
(define (assemble/write-invoke stmts op)
  (let ([basic-blocks (fracture stmts)])
    (fprintf op "(function(MACHINE, success, fail, params) {\n")
    (fprintf op "var param;\n")
    (fprintf op "var RUNTIME = plt.runtime;\n")
    (for-each (lambda: ([basic-block : BasicBlock])
                       (displayln (assemble-basic-block basic-block) op)
                       (newline op))
              basic-blocks)
    (write-linked-label-attributes stmts op)
    (fprintf op "MACHINE.params.currentErrorHandler = fail;\n")
    (fprintf op "MACHINE.params.currentSuccessHandler = success;\n")
    (fprintf op #<<EOF
for (param in params) {
    if (params.hasOwnProperty(param)) {
        MACHINE.params[param] = params[param];
    }
}
EOF
             )
    (fprintf op "RUNTIME.trampoline(MACHINE, ~a); })"
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
            [else
             (let ([first-stmt (car stmts)])
               (: do-on-label (Symbol -> (Listof BasicBlock)))
               (define (do-on-label label-name)
                 (cond
                    [(member label-name jump-targets)
                     (loop label-name
                           '()
                           (cons (make-BasicBlock 
                                  name  
                                  (if last-stmt-goto? 
                                      (reverse acc)
                                      (reverse (append `(,(make-GotoStatement (make-Label label-name)))
                                                       acc))))
                                 basic-blocks)
                           (cdr stmts)
                           last-stmt-goto?)]
                    [else
                     (loop name
                           acc
                           basic-blocks
                           (cdr stmts)
                           last-stmt-goto?)]))
               (cond
                 [(symbol? first-stmt)
                  (do-on-label first-stmt)]
                 [(LinkedLabel? first-stmt)
                  (do-on-label (LinkedLabel-label first-stmt))]
                 [else
                  (loop name
                        (cons first-stmt acc)
                        basic-blocks
                        (cdr stmts)
                        (GotoStatement? (car stmts)))]))]))))


(: write-linked-label-attributes ((Listof Statement) Output-Port -> 'ok))
(define (write-linked-label-attributes stmts op)
  (cond
    [(empty? stmts)
     'ok]
    [else
     (let ([stmt (first stmts)])

       (define (next) (write-linked-label-attributes (rest stmts) op))

       (cond
         [(symbol? stmt)
          (next)]
         [(LinkedLabel? stmt)
          (fprintf op "~a.multipleValueReturn = ~a;\n" 
                   (LinkedLabel-label stmt)
                   (LinkedLabel-linked-to stmt))
          (next)]
         [(AssignImmediateStatement? stmt)
          (next)]
         [(AssignPrimOpStatement? stmt)
          (next)]
         [(PerformStatement? stmt)
          (next)]
         [(TestAndBranchStatement? stmt)
          (next)]
         [(GotoStatement? stmt)
          (next)]
         [(PushEnvironment? stmt)
          (next)]
         [(PopEnvironment? stmt)
          (next)]
         [(PushImmediateOntoEnvironment? stmt)
          (next)]
         [(PushControlFrame? stmt)
          (next)]
         [(PushControlFrame/Prompt? stmt)
          (next)]
         [(PopControlFrame? stmt)
          (next)]
         [(PopControlFrame/Prompt? stmt)
          (next)]))]))
       

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
       empty]
      [(SubtractArg? an-input)
       (append (collect-input (SubtractArg-lhs an-input))
               (collect-input (SubtractArg-rhs an-input)))]))
  
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
      [(MakeCompiledProcedureShell? op)
       (list (MakeCompiledProcedureShell-label op))]
      [(ApplyPrimitiveProcedure? op)
       empty]
      [(GetControlStackLabel? op)
       empty]
      [(GetControlStackLabel/MultipleValueReturn? op)
       empty]
      [(CaptureEnvironment? op)
       empty]
      [(CaptureControl? op)
       empty]
      [(MakeBoxedEnvironmentValue? op)
       empty]
      [(CallKernelPrimitiveProcedure? op)
       empty]))
  
  (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
  (define (collect-primitive-command op)
    (cond
      [(CheckToplevelBound!? op)
       empty]
      [(CheckClosureArity!? op)
       empty]
      [(CheckPrimitiveArity!? op)
       empty]
      [(ExtendEnvironment/Prefix!? op)
       empty]
      [(InstallClosureValues!? op)
       empty]
      [(RestoreEnvironment!? op)
       empty]
      [(RestoreControl!? op)
       empty]
      [(SetFrameCallee!? op)
       empty]
      [(SpliceListIntoStack!? op)
       empty]
      [(UnspliceRestFromStack!? op)
       empty]
      [(FixClosureShellMap!? op)
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
                            [(LinkedLabel? stmt)
                             (list (LinkedLabel-label stmt)
                                   (LinkedLabel-linked-to stmt))]
                            [(AssignImmediateStatement? stmt)
                             (let: ([v : OpArg (AssignImmediateStatement-value stmt)])
                                   (collect-input v))]
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
                            [(PushImmediateOntoEnvironment? stmt)
                             (collect-input (PushImmediateOntoEnvironment-value stmt))]
                            [(PushControlFrame? stmt)
                             (label->labels (PushControlFrame-label stmt))]
                            [(PushControlFrame/Prompt? stmt)
                             (label->labels (PushControlFrame/Prompt-label stmt))]
                            [(PopControlFrame? stmt)
                             empty]
                            [(PopControlFrame/Prompt? stmt)
                             empty])
                          (loop (rest stmts))))]))))

(: label->labels ((U Symbol LinkedLabel) -> (Listof Symbol)))
(define (label->labels label)
  (cond
    [(symbol? label)
     (list label)]
    [(LinkedLabel? label)
     (list (LinkedLabel-label label)
           (LinkedLabel-linked-to label))]))


;; assemble-basic-block: basic-block -> string
(: assemble-basic-block (BasicBlock -> String))
(define (assemble-basic-block a-basic-block)
  (format "var ~a=function(MACHINE){\nif(--MACHINE.callsBeforeTrampoline < 0) { throw ~a; }\n~a};"
          (BasicBlock-name a-basic-block)
          (BasicBlock-name a-basic-block)
          (string-join (map assemble-statement (BasicBlock-stmts a-basic-block))
                       "\n")))




(: assemble-statement (UnlabeledStatement -> String))
;; Generates the code to assemble a statement.
(define (assemble-statement stmt)
  (string-append 
   (if (current-emit-debug-trace?)
       (format "if (typeof(window.console) !== 'undefined' && typeof(console.log) === 'function') { console.log(~s);\n}"
               (format "~a" stmt))
       "")
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
                (format "if (! ~a) { ~a }"
                        (assemble-reg (make-Reg (TestAndBranchStatement-register stmt)))
                        (assemble-jump (make-Label (TestAndBranchStatement-label stmt))))]
               [(eq? test 'one?)
                (format "if (~a === 1) { ~a }"
                        (assemble-reg (make-Reg (TestAndBranchStatement-register stmt)))
                        (assemble-jump (make-Label (TestAndBranchStatement-label stmt))))]
               [(eq? test 'primitive-procedure?)
                (format "if (typeof(~a) === 'function') { ~a };"
                        (assemble-reg (make-Reg (TestAndBranchStatement-register stmt)))
                        (assemble-jump (make-Label (TestAndBranchStatement-label stmt))))]))]
     
     [(GotoStatement? stmt)
      (assemble-jump (GotoStatement-target stmt))]

     [(PushControlFrame? stmt)
      (format "MACHINE.control.push(new RUNTIME.CallFrame(~a, MACHINE.proc));" 
              (let ([label (PushControlFrame-label stmt)])
                (cond
                  [(symbol? label) label]
                  [(LinkedLabel? label) (LinkedLabel-label label)])))]

     [(PushControlFrame/Prompt? stmt)
      ;; fixme: use a different frame structure
      (format "MACHINE.control.push(new RUNTIME.PromptFrame(~a, ~a));" 
              (let ([label (PushControlFrame/Prompt-label stmt)])
                (cond
                  [(symbol? label) label]
                  [(LinkedLabel? label) (LinkedLabel-label label)]))

              (let ([tag (PushControlFrame/Prompt-tag stmt)])
                (cond
                  [(DefaultContinuationPromptTag? tag)
                   (assemble-default-continuation-prompt-tag)]
                  [(OpArg? tag)
                   (assemble-oparg tag)])))]
     
     [(PopControlFrame? stmt)
      "MACHINE.control.pop();"]
     
     [(PopControlFrame/Prompt? stmt)
      "MACHINE.control.pop();"]

     [(PushEnvironment? stmt)
      (if (= (PushEnvironment-n stmt) 0)
          ""
          (format "MACHINE.env.push(~a);" (string-join
                                           (build-list (PushEnvironment-n stmt) 
                                                       (lambda: ([i : Natural])
                                                                (if (PushEnvironment-unbox? stmt)
                                                                    "[undefined]"
                                                                    "undefined")))
                                           ", ")))]
     [(PopEnvironment? stmt)
      (let ([skip (PopEnvironment-skip stmt)])
        (cond
          [(and (Const? skip) (= (ensure-natural (Const-const skip)) 0))
           (format "MACHINE.env.length = MACHINE.env.length - ~a;"
                   (assemble-oparg (PopEnvironment-n stmt)))]
          [else
           (format "MACHINE.env.splice(MACHINE.env.length - (~a + ~a), ~a);"
                   (assemble-oparg (PopEnvironment-skip stmt))
                   (assemble-oparg (PopEnvironment-n stmt))
                   (assemble-oparg (PopEnvironment-n stmt)))]))]
     
     [(PushImmediateOntoEnvironment? stmt)
      (format "MACHINE.env.push(~a);"
              (let: ([val-string : String
                                 (cond [(PushImmediateOntoEnvironment-box? stmt)
                                        (format "[~a]" (assemble-oparg (PushImmediateOntoEnvironment-value stmt)))]
                                       [else
                                        (assemble-oparg (PushImmediateOntoEnvironment-value stmt))])])
                    val-string))])))


(: ensure-natural (Any -> Natural))
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))


(: assemble-jump ((U Label Reg) -> String))
(define (assemble-jump target)
  (format "return (~a)(MACHINE);" (assemble-location target)))








(: assemble-env-reference/closure-capture (Natural -> String))
;; When we're capturing the values for a closure, we need to not unbox
;; lexical references: they must remain boxes.  So all we need is 
;; the depth into the environment.
(define (assemble-env-reference/closure-capture depth)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          depth))


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
     (format "new RUNTIME.Closure(~a, ~a, [~a], ~a)"
             (MakeCompiledProcedure-label op)
             (assemble-arity (MakeCompiledProcedure-arity op))
             (string-join (map assemble-env-reference/closure-capture 
                               ;; The closure values are in reverse order
                               ;; to make it easier to push, in bulk, into
                               ;; the environment (which is also in reversed order)
                               ;; during install-closure-values.
                               (reverse (MakeCompiledProcedure-closed-vals op)))
                          ", ")
             (assemble-display-name (MakeCompiledProcedure-display-name op)))]
    
    [(MakeCompiledProcedureShell? op)
     (format "new RUNTIME.Closure(~a, ~a, undefined, ~a)"
             (MakeCompiledProcedureShell-label op)
             (assemble-arity (MakeCompiledProcedureShell-arity op))
             (assemble-display-name (MakeCompiledProcedureShell-display-name op)))]
    
    [(ApplyPrimitiveProcedure? op)
     (format "MACHINE.proc(MACHINE)")]
    
    [(GetControlStackLabel? op)
     (format "MACHINE.control[MACHINE.control.length-1].label")]

    [(GetControlStackLabel/MultipleValueReturn? op)
     (format "MACHINE.control[MACHINE.control.length-1].label.multipleValueReturn")]
  
    [(CaptureEnvironment? op)
     (format "MACHINE.env.slice(0, MACHINE.env.length - ~a)"
             (CaptureEnvironment-skip op))]
    
    [(CaptureControl? op)
     (format "RUNTIME.captureControl(MACHINE, ~a, ~a)"
             (CaptureControl-skip op)
             (let ([tag (CaptureControl-tag op)])
               (cond [(DefaultContinuationPromptTag? tag)
                      (assemble-default-continuation-prompt-tag)]
                     [(OpArg? tag)
                      (assemble-oparg tag)])))]

    
    [(MakeBoxedEnvironmentValue? op)
     (format "[MACHINE.env[MACHINE.env.length - 1 - ~a]]"
             (MakeBoxedEnvironmentValue-depth op))]

    [(CallKernelPrimitiveProcedure? op)
     (open-code-kernel-primitive-procedure op)]))


(define-predicate natural? Natural)

(: assemble-arity (Arity -> String))
(define (assemble-arity an-arity)
  (cond
    [(natural? an-arity)
     (format "~a" an-arity)]
    [(ArityAtLeast? an-arity)
     (format "(new RUNTIME.ArityAtLeast(~a))" (ArityAtLeast-value an-arity))]
    [(listof-atomic-arity? an-arity)
     (assemble-listof-assembled-values
      (map (lambda: ([atomic-arity : (U Natural ArityAtLeast)])
                    (cond
                      [(natural? atomic-arity)
                       (format "~a" an-arity)]
                      [(ArityAtLeast? an-arity)
                       (format "(new RUNTIME.ArityAtLeast(~a))" (ArityAtLeast-value an-arity))]
                      ;; Can't seem to make the type checker happy without this...
                      [else (error 'assemble-arity)]))
           an-arity))]))


(: assemble-default-continuation-prompt-tag (-> String))
(define (assemble-default-continuation-prompt-tag)
  "RUNTIME.DEFAULT_CONTINUATION_PROMPT_TAG")





(: assemble-op-statement (PrimitiveCommand -> String))
(define (assemble-op-statement op)  
  (cond 
    
    [(CheckToplevelBound!? op)
     (format "if (MACHINE.env[MACHINE.env.length - 1 - ~a][~a] === undefined) { throw new Error(\"Not bound: \" + MACHINE.env[MACHINE.env.length - 1 - ~a].names[~a]); }"
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op)
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op))]
    
    [(CheckClosureArity!? op)
     (format "if (! (MACHINE.proc instanceof RUNTIME.Closure && RUNTIME.isArityMatching(MACHINE.proc.arity, ~a))) { if (! (MACHINE.proc instanceof RUNTIME.Closure)) { throw new Error(\"not a closure\"); } else { throw new Error(\"arity failure:\" + MACHINE.proc.displayName); } }"
             (assemble-oparg (CheckClosureArity!-arity op)))]
    
    [(CheckPrimitiveArity!? op)
     (format "if (! (typeof(MACHINE.proc) === 'function'  && RUNTIME.isArityMatching(MACHINE.proc.arity, ~a))) { if (! (typeof(MACHINE.proc) === 'function')) { throw new Error(\"not a primitive procedure\"); } else { throw new Error(\"arity failure:\" + MACHINE.proc.displayName); } }"
             (assemble-oparg (CheckPrimitiveArity!-arity op)))]
     
    
    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof (U Symbol False ModuleVariable)) (ExtendEnvironment/Prefix!-names op)])
           (format "MACHINE.env.push([~a]);  MACHINE.env[MACHINE.env.length-1].names = [~a];"
                   (string-join (map (lambda: ([n : (U Symbol False ModuleVariable)])
                                              (cond [(symbol? n)
                                                     (format "MACHINE.params.currentNamespace[~s] || MACHINE.primitives[~s]"
                                                             (symbol->string n) 
                                                             (symbol->string n))]
                                                    [(eq? n #f)
                                                     "false"]
                                                    [(ModuleVariable? n)
                                                     (format "MACHINE.primitives[~s]"
                                                             (symbol->string (ModuleVariable-name n)))]))
                                     names)
                                ",")
                   (string-join (map (lambda: ([n : (U Symbol False ModuleVariable)])
                                              (cond
                                                [(symbol? n)
                                                 (format "~s" (symbol->string n))]
                                                [(eq? n #f)
                                                 "false"]
                                                [(ModuleVariable? n)
                                                 (format "~s" (symbol->string (ModuleVariable-name n)))]))
                                     names)
                                ",")))]
    
    [(InstallClosureValues!? op)
     "MACHINE.env.splice.apply(MACHINE.env, [MACHINE.env.length, 0].concat(MACHINE.proc.closedVals));"]
    [(RestoreEnvironment!? op)
     "MACHINE.env = MACHINE.env[MACHINE.env.length - 2].slice(0);"]
    [(RestoreControl!? op)
     (format "RUNTIME.restoreControl(MACHINE, ~a);"
             (let ([tag (RestoreControl!-tag op)])
               (cond
                 [(DefaultContinuationPromptTag? tag)
                  (assemble-default-continuation-prompt-tag)]
                 [(OpArg? tag)
                  (assemble-oparg tag)])))]
    [(FixClosureShellMap!? op)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a].closedVals = [~a]"
             (FixClosureShellMap!-depth op)
              (string-join (map assemble-env-reference/closure-capture 
                               ;; The closure values are in reverse order
                               ;; to make it easier to push, in bulk, into
                               ;; the environment (which is also in reversed order)
                               ;; during install-closure-values.
                               (reverse (FixClosureShellMap!-closed-vals op)))
                          ", "))]
    [(SetFrameCallee!? op)
     (format "MACHINE.control[MACHINE.control.length-1].proc = ~a;"
             (assemble-oparg (SetFrameCallee!-proc op)))]
    [(SpliceListIntoStack!? op)
     (format "RUNTIME.spliceListIntoStack(MACHINE, ~a);"
             (assemble-oparg (SpliceListIntoStack!-depth op)))]
    [(UnspliceRestFromStack!? op)
     (format "RUNTIME.unspliceRestFromStack(MACHINE, ~a, ~a);"
             (assemble-oparg (UnspliceRestFromStack!-depth op))
             (assemble-oparg (UnspliceRestFromStack!-length op)))]))



(: assemble-location ((U Reg Label) -> String))
(define (assemble-location a-location)
  (cond
    [(Reg? a-location)
     (assemble-reg a-location)]
    [(Label? a-location)
     (assemble-label a-location)]))


