#lang typed/racket/base
(require "il-structs.rkt"
         "lexical-structs.rkt"
         "helpers.rkt"
         "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
         "assemble-expression.rkt"
         "assemble-perform-statement.rkt"
         "collect-jump-targets.rkt"
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
  (let: ([basic-blocks : (Listof BasicBlock) (fracture stmts)])
        (fprintf op "(function(MACHINE, success, fail, params) {\n")
        (fprintf op "var param;\n")
        (fprintf op "var RUNTIME = plt.runtime;\n")
        (for-each
         (lambda: ([basic-block : BasicBlock])
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
                 (assemble-label (make-Label (BasicBlock-name (first basic-blocks)))))))




;; fracture: (listof stmt) -> (listof basic-block)
(: fracture ((Listof Statement) -> (Listof BasicBlock)))
(define (fracture stmts)
  (let*: ([first-block-label : Symbol (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (first stmts)
                                          (make-label 'start))]
          [stmts : (Listof Statement) (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (rest stmts)
                                          stmts)]
          [jump-targets : (Listof Symbol)
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
                  (let: ([first-stmt : Statement (car stmts)])
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
     (let: ([stmt : Statement (first stmts)])
           
           (define (next) (write-linked-label-attributes (rest stmts) op))
           
           (cond
             [(symbol? stmt)
              (next)]
             [(LinkedLabel? stmt)
              (fprintf op "~a.multipleValueReturn = ~a;\n" 
                       (assemble-label (make-Label (LinkedLabel-label stmt)))
                       (assemble-label (make-Label (LinkedLabel-linked-to stmt))))
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
             [(PushControlFrame/Generic? stmt)
              (next)]
             [(PushControlFrame/Call? stmt)
              (next)]
             [(PushControlFrame/Prompt? stmt)
              (next)]
             [(PopControlFrame? stmt)
              (next)]))]))




;; assemble-basic-block: basic-block -> string
(: assemble-basic-block (BasicBlock -> String))
(define (assemble-basic-block a-basic-block)
  (format "var ~a=function(MACHINE){\nif(--MACHINE.callsBeforeTrampoline < 0) { throw ~a; }\n~a};"
          (assemble-label (make-Label (BasicBlock-name a-basic-block)))
          (assemble-label (make-Label (BasicBlock-name a-basic-block)))
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
      (let: ([t : String (assemble-target (AssignImmediateStatement-target stmt))]
             [v : OpArg (AssignImmediateStatement-value stmt)])
            (format "~a = ~a;" t (assemble-oparg v)))]
     
     [(AssignPrimOpStatement? stmt)
      (format "~a=~a;" 
              (assemble-target (AssignPrimOpStatement-target stmt))
              (assemble-op-expression (AssignPrimOpStatement-op stmt)))]
     
     [(PerformStatement? stmt)
      (assemble-op-statement (PerformStatement-op stmt))]
     
     [(TestAndBranchStatement? stmt)
      (let*: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)]
              [jump : String (assemble-jump 
                       (make-Label (TestAndBranchStatement-label stmt)))])
             ;; to help localize type checks, we add a type annotation here.
             (ann (cond
                    [(TestFalse? test)
                     (format "if (~a === false) { ~a }"
                             (assemble-oparg (TestFalse-operand test))
                             jump)]
                    [(TestOne? test)
                     (format "if (~a === 1) { ~a }"
                             (assemble-oparg (TestOne-operand test))
                             jump)]
                    [(TestZero? test)
                     (format "if (~a === 0) { ~a }"
                             (assemble-oparg (TestZero-operand test))
                             jump)]
                    [(TestPrimitiveProcedure? test)
                     (format "if (typeof(~a) === 'function') { ~a }"
                             (assemble-oparg (TestPrimitiveProcedure-operand test))
                             jump)]
                    [(TestClosureArityMismatch? test)
                     (format "if (! RUNTIME.isArityMatching((~a).arity, ~a)) { ~a }"
                             (assemble-oparg (TestClosureArityMismatch-closure test))
                             (assemble-oparg (TestClosureArityMismatch-n test))
                             jump)])
                  String))]
     
     [(GotoStatement? stmt)
      (assemble-jump (GotoStatement-target stmt))]
     
     [(PushControlFrame/Generic? stmt)
      "MACHINE.control.push(new RUNTIME.Frame());"]
     
     [(PushControlFrame/Call? stmt)
      (format "MACHINE.control.push(new RUNTIME.CallFrame(~a, MACHINE.proc));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Call-label stmt)])
                    (cond
                      [(symbol? label) 
                       (assemble-label (make-Label label))]
                      [(LinkedLabel? label) 
                       (assemble-label (make-Label (LinkedLabel-label label)))])))]
     
     [(PushControlFrame/Prompt? stmt)
      ;; fixme: use a different frame structure
      (format "MACHINE.control.push(new RUNTIME.PromptFrame(~a, ~a));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Prompt-label stmt)])
                    (cond
                      [(symbol? label) 
                       (assemble-label (make-Label label))]
                      [(LinkedLabel? label) 
                       (assemble-label (make-Label (LinkedLabel-label label)))]))
              
              (let: ([tag : (U DefaultContinuationPromptTag OpArg)
                          (PushControlFrame/Prompt-tag stmt)])
                    (cond
                      [(DefaultContinuationPromptTag? tag)
                       (assemble-default-continuation-prompt-tag)]
                      [(OpArg? tag)
                       (assemble-oparg tag)])))]
     
     [(PopControlFrame? stmt)
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
      (let: ([skip : OpArg (PopEnvironment-skip stmt)])
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


(define-predicate natural? Natural)

(: ensure-natural (Any -> Natural))
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))


