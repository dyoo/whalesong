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
  (let* ([first-block-label (make-label 'start)]
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
      [(LookupLexicalAddress? op)
       empty]
      [(LookupToplevelAddress? op)
       empty]
      [(GetControlStackLabel? op)
       empty]))

  (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
  (define (collect-primitive-command op)
    (cond
      [(SetToplevel!? op) 
       empty]
      [(CheckToplevelBound!? op)
       empty]
      [(CheckClosureArity!? op)
       empty]
      [(ExtendEnvironment/Prefix!? op)
       empty]
      [(InstallClosureValues!? op)
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
                         empty]
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




(: assemble-statement (UnlabeledStatement -> String))
;; Generates the code to assemble a statement.
(define (assemble-statement stmt)
  (cond
    [(AssignImmediateStatement? stmt)
     (let ([t (assemble-target (AssignImmediateStatement-target stmt))]
           [v (AssignImmediateStatement-value stmt)])
       (format "~a = ~a;"
               t
               (cond 
                 [(Reg? v)
                  (assemble-reg v)]
                 [(Label? v)
                  (assemble-label v)]
                 [(Const? v)
                  (assemble-const v)]
                 [(EnvLexicalReference? v)
                  (assemble-lexical-reference v)]
                 [(EnvWholePrefixReference? v)
                  (assemble-whole-prefix-reference v)])))]
    
    [(AssignPrimOpStatement? stmt)
     (format "MACHINE.~a=~a;" 
             (AssignPrimOpStatement-target stmt)
             (assemble-op-expression (AssignPrimOpStatement-op stmt)))]
    
    [(PerformStatement? stmt)
     (assemble-op-statement (PerformStatement-op stmt))]
    
    [(TestAndBranchStatement? stmt)
     (error 'assemble-stmt)
     #;(format "if(~a){return ~a();}"
             (assemble-op-expression (TestAndBranchStatement-op stmt)
                                     (list (make-Reg (TestAndBranchStatement-register stmt))))
             (assemble-location (make-Label (TestAndBranchStatement-label stmt))))]

    [(GotoStatement? stmt)
     (format "return ~a();"
             (assemble-location (GotoStatement-target stmt)))]
    [(PushControlFrame? stmt)
     (format "MACHINE.control.push(~a);" (PushControlFrame-label stmt))]
    [(PopControlFrame? stmt)
     "MACHINE.control.pop();"]
    [(PushEnvironment? stmt)
     (format "MACHINE.env.push(~a);" (string-join
                                      (build-list (PushEnvironment-n stmt) (lambda: ([i : Natural])
                                                                                    "undefined"))
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
     (assemble-lexical-reference target)]))




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
              [(empty? val)
               (format "undefined")]
              [else
               (format "~s" val)])))


(: assemble-lexical-reference (EnvLexicalReference -> String))
(define (assemble-lexical-reference a-lex-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          (EnvLexicalReference-depth a-lex-ref)))

(: assemble-whole-prefix-reference (EnvWholePrefixReference -> String))
(define (assemble-whole-prefix-reference a-prefix-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          (EnvWholePrefixReference-depth a-prefix-ref)))

(: assemble-env-reference (EnvReference -> String))
(define (assemble-env-reference ref)
  (cond
    [(EnvLexicalReference? ref)
     (assemble-lexical-reference ref)]
    [(EnvWholePrefixReference? ref)
     (assemble-whole-prefix-reference ref)]))


(: assemble-op-expression (PrimitiveOperator -> String))
(define (assemble-op-expression op)
  (cond
    [(GetCompiledProcedureEntry? op)
     "proc.label"]
    
    [(MakeCompiledProcedure? op)
     (format "new Closure(~a, ~a, [~a], ~s)"
             (MakeCompiledProcedure-label op)
             (MakeCompiledProcedure-arity op)
             (string-join (map assemble-env-reference 
                               (MakeCompiledProcedure-closed-vals op))
                          ", ")
             (symbol->string (MakeCompiledProcedure-label op)))]
    
    [(ApplyPrimitiveProcedure? op)
     (format "MACHINE.proc(~a, ~a)"
             (ApplyPrimitiveProcedure-arity op)
             (ApplyPrimitiveProcedure-label op))]

    [(LookupLexicalAddress? op)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a][~a]"
             (LookupLexicalAddress-depth op))]

    [(LookupToplevelAddress? op)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a][~a]"
             (LookupToplevelAddress-depth op)
             (LookupToplevelAddress-pos op))]

    [(GetControlStackLabel? op)
     (format "MACHINE.control[MACHINE.control.length-1].label")]
    
    #;[(compiled-procedure-env)
     #;(format "(~a.env)" (assemble-input (first inputs)))]
    #;[(make-compiled-procedure)
     (format "(new Closure(~a, ~a))"
             (second assembled-inputs)
             (first assembled-inputs))]
    #;[(false?)
     (format "(!(~a))" (assemble-input (first inputs)))]
    #;[(cons)
     (format "[~a]" (string-join (map assemble-input inputs) ","))]
    #;[(list)
     (cond [(empty? inputs)
            "undefined"]
           [else
            (let: loop : String ([assembled-inputs : (Listof String) assembled-inputs])
                  (cond
                    [(empty? assembled-inputs)
                     "undefined"]
                    [else
                     (format "[~a, ~a]"
                             (first assembled-inputs)
                             (loop (rest assembled-inputs)))]))])]
    #;[(apply-primitive-procedure)
     (format "~a(~a)"
             (first assembled-inputs)
             ;; FIXME: this doesn't look quite right...
             (third assembled-inputs))]
    #;[(lexical-address-lookup)
     (format "(~a).valss[~a][~a]"
             (third assembled-inputs)
             (first assembled-inputs)
             (second assembled-inputs))]
    #;[(toplevel-lookup)
     (let ([depth (first assembled-inputs)]
           [pos (second assembled-inputs)]
           [name (third assembled-inputs)]
           [env (fourth assembled-inputs)])
       (format "(~a).valss[~a][~a]" env depth pos))]
    #;[(primitive-procedure?)
     (format "(typeof(~a) === 'function')"
             (first assembled-inputs))]
    #;[(extend-environment)
     (format "new ExtendedEnvironment(~a, ~a)"
             (second assembled-inputs)
             (first assembled-inputs))]
    #;[(extend-environment/prefix)
     (format "new ExtendedPrefixEnvironment(~a, ~a)"
             (second assembled-inputs)
             (first assembled-inputs))]
    #;[(read-control-label)
     "fixme"]
    ))


(: assemble-op-statement (PrimitiveCommand -> String))
(define (assemble-op-statement op)  
  (cond 
    [(SetToplevel!? op)
     (error 'assemble-op-statement)
     #;(let ([depth (first assembled-inputs)]
             [pos (second assembled-inputs)]
             [name (third assembled-inputs)]
             [env (fourth assembled-inputs)]
             [val (fifth assembled-inputs)])
         (format "(~a).valss[~a][~a] = ~a;"
                 env
                 depth
                 pos
                 val))]
    
    [(CheckToplevelBound!? op)
     (error 'assemble-op-statement)
     #;(let ([depth (first assembled-inputs)]
             [pos (second assembled-inputs)]
             [name (third assembled-inputs)]
             [env (fourth assembled-inputs)])
         (format "if ((~a).valss[~a][~a] === undefined) { throw new Error(\"Not bound: \" + ~a); }"
                 env
                 depth
                 pos
                 name))]
    [(CheckClosureArity!? op)
     ;; fixme
     (error 'assemble-op-statement)]
    
    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof Symbol) (ExtendEnvironment/Prefix!-names op)])
           (format "MACHINE.env.push([~a]);"
                   (string-join (map (lambda: ([n : Symbol])
                                              (format "MACHINE.params.currentNamespace[~s] || Primitives[~s]"
                                                      (symbol->string n) 
                                                      (symbol->string n)))
                                     names)
                                ",")))]
    
    [(InstallClosureValues!? op)
     (error 'assemble-op-statement)]))
      
      




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