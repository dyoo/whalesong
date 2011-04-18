#lang typed/racket/base
(require "il-structs.rkt"
         "lexical-structs.rkt"
         "helpers.rkt"
	 "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-open-coded.rkt"
	 "assemble-expression.rkt"
	 "assemble-perform-statement.rkt"
         racket/string
         racket/list)

;; (provide assemble/write-invoke
;;          fracture
;;          assemble-basic-block
;;          assemble-statement)


;; Parameter that controls the generation of a trace.
(define current-emit-debug-trace? (make-parameter #f))



;; (: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
;; ;; Writes out the JavaScript code that represents the anonymous invocation expression.
;; (define (assemble/write-invoke stmts op)
;;   (let: ([basic-blocks : (Listof BasicBlock) (fracture stmts)])
;;     (fprintf op "(function(MACHINE, success, fail, params) {\n")
;;     (fprintf op "var param;\n")
;;     (fprintf op "var RUNTIME = plt.runtime;\n")
;;     ((inst for-each BasicBlock Void)
;;      (lambda: ([basic-block : BasicBlock])
;; 	      (displayln (assemble-basic-block basic-block) op)
;; 	      (newline op))
;;      basic-blocks)
;;     (write-linked-label-attributes stmts op)
;;     (fprintf op "MACHINE.params.currentErrorHandler = fail;\n")
;;     (fprintf op "MACHINE.params.currentSuccessHandler = success;\n")
;;     (fprintf op #<<EOF
;; for (param in params) {
;;     if (params.hasOwnProperty(param)) {
;;         MACHINE.params[param] = params[param];
;;     }
;; }
;; EOF
;;              )
;;     (fprintf op "RUNTIME.trampoline(MACHINE, ~a); })"
;;              (BasicBlock-name (first basic-blocks)))))




;; ;; fracture: (listof stmt) -> (listof basic-block)
;; (: fracture ((Listof Statement) -> (Listof BasicBlock)))
;; (define (fracture stmts)
;;   (let*: ([first-block-label : Symbol (if (and (not (empty? stmts))
;; 					       (symbol? (first stmts)))
;; 					  (first stmts)
;; 					  (make-label 'start))]
;; 	  [stmts : (Listof Statement) (if (and (not (empty? stmts))
;; 					       (symbol? (first stmts)))
;; 					  (rest stmts)
;; 					  stmts)]
;; 	  [jump-targets : (Listof Symbol)
;; 			(cons first-block-label (collect-general-jump-targets stmts))])
;;     (let: loop : (Listof BasicBlock)
;;           ([name : Symbol first-block-label]
;;            [acc : (Listof UnlabeledStatement) '()]
;;            [basic-blocks  : (Listof BasicBlock) '()]
;;            [stmts : (Listof Statement) stmts]
;;            [last-stmt-goto? : Boolean #f])
;;           (cond
;;             [(null? stmts)
;;              (reverse (cons (make-BasicBlock name (reverse acc))
;;                             basic-blocks))]
;;             [else
;;              (let: ([first-stmt : Statement (car stmts)])
;;                (: do-on-label (Symbol -> (Listof BasicBlock)))
;;                (define (do-on-label label-name)
;;                  (cond
;;                     [(member label-name jump-targets)
;;                      (loop label-name
;;                            '()
;;                            (cons (make-BasicBlock 
;;                                   name  
;;                                   (if last-stmt-goto? 
;;                                       (reverse acc)
;;                                       (reverse (append `(,(make-GotoStatement (make-Label label-name)))
;;                                                        acc))))
;;                                  basic-blocks)
;;                            (cdr stmts)
;;                            last-stmt-goto?)]
;;                     [else
;;                      (loop name
;;                            acc
;;                            basic-blocks
;;                            (cdr stmts)
;;                            last-stmt-goto?)]))
;;                (cond
;;                  [(symbol? first-stmt)
;;                   (do-on-label first-stmt)]
;;                  [(LinkedLabel? first-stmt)
;;                   (do-on-label (LinkedLabel-label first-stmt))]
;;                  [else
;;                   (loop name
;;                         (cons first-stmt acc)
;;                         basic-blocks
;;                         (cdr stmts)
;;                         (GotoStatement? (car stmts)))]))]))))


;; (: write-linked-label-attributes ((Listof Statement) Output-Port -> 'ok))
;; (define (write-linked-label-attributes stmts op)
;;   (cond
;;     [(empty? stmts)
;;      'ok]
;;     [else
;;      (let: ([stmt : Statement (first stmts)])

;;        (define (next) (write-linked-label-attributes (rest stmts) op))

;;        (cond
;;          [(symbol? stmt)
;;           (next)]
;;          [(LinkedLabel? stmt)
;;           (fprintf op "~a.multipleValueReturn = ~a;\n" 
;;                    (LinkedLabel-label stmt)
;;                    (LinkedLabel-linked-to stmt))
;;           (next)]
;;          [(AssignImmediateStatement? stmt)
;;           (next)]
;;          [(AssignPrimOpStatement? stmt)
;;           (next)]
;;          [(PerformStatement? stmt)
;;           (next)]
;;          [(TestAndBranchStatement? stmt)
;;           (next)]
;;          [(GotoStatement? stmt)
;;           (next)]
;;          [(PushEnvironment? stmt)
;;           (next)]
;;          [(PopEnvironment? stmt)
;;           (next)]
;;          [(PushImmediateOntoEnvironment? stmt)
;;           (next)]
;;          [(PushControlFrame/Generic? stmt)
;;           (next)]
;;          [(PushControlFrame/Call? stmt)
;;           (next)]
;;          [(PushControlFrame/Prompt? stmt)
;;           (next)]
;;          [(PopControlFrame? stmt)
;;           (next)]))]))
       

;; ;; collect-general-jump-targets: (listof stmt) -> (listof label)
;; ;; collects all the labels that are potential targets for GOTOs or branches.
;; (: collect-general-jump-targets ((Listof Statement) -> (Listof Symbol)))
;; (define (collect-general-jump-targets stmts)
;;   (: collect-input (OpArg -> (Listof Symbol)))
;;   (define (collect-input an-input)
;;     (cond
;;       [(Reg? an-input)
;;        empty]
;;       [(Const? an-input)
;;        empty]
;;       [(Label? an-input)
;;        (list (Label-name an-input))]
;;       [(EnvLexicalReference? an-input)
;;        empty]
;;       [(EnvPrefixReference? an-input)
;;        empty]
;;       [(EnvWholePrefixReference? an-input)
;;        empty]
;;       [(SubtractArg? an-input)
;;        (append (collect-input (SubtractArg-lhs an-input))
;;                (collect-input (SubtractArg-rhs an-input)))]
;;       [(ControlStackLabel? an-input)
;;        empty]
;;       [(ControlStackLabel/MultipleValueReturn? an-input)
;;        empty]))
  
;;   (: collect-location ((U Reg Label) -> (Listof Symbol)))
;;   (define (collect-location a-location)
;;     (cond
;;       [(Reg? a-location)
;;        empty]
;;       [(Label? a-location)
;;        (list (Label-name a-location))]))
  
;;   (: collect-primitive-operator (PrimitiveOperator -> (Listof Symbol)))
;;   (define (collect-primitive-operator op)
;;     (cond
;;       [(GetCompiledProcedureEntry? op)
;;        empty]
;;       [(MakeCompiledProcedure? op)
;;        (list (MakeCompiledProcedure-label op))]
;;       [(MakeCompiledProcedureShell? op)
;;        (list (MakeCompiledProcedureShell-label op))]
;;       [(ApplyPrimitiveProcedure? op)
;;        empty]
;;       [(CaptureEnvironment? op)
;;        empty]
;;       [(CaptureControl? op)
;;        empty]
;;       [(MakeBoxedEnvironmentValue? op)
;;        empty]
;;       [(CallKernelPrimitiveProcedure? op)
;;        empty]))
  
;;   (: collect-primitive-command (PrimitiveCommand -> (Listof Symbol)))
;;   (define (collect-primitive-command op)
;;     (cond
;;       [(CheckToplevelBound!? op)
;;        empty]
;;       [(CheckClosureArity!? op)
;;        empty]
;;       [(CheckPrimitiveArity!? op)
;;        empty]
;;       [(ExtendEnvironment/Prefix!? op)
;;        empty]
;;       [(InstallClosureValues!? op)
;;        empty]
;;       [(RestoreEnvironment!? op)
;;        empty]
;;       [(RestoreControl!? op)
;;        empty]
;;       [(SetFrameCallee!? op)
;;        empty]
;;       [(SpliceListIntoStack!? op)
;;        empty]
;;       [(UnspliceRestFromStack!? op)
;;        empty]
;;       [(FixClosureShellMap!? op)
;;        empty]
;;       [(InstallContinuationMarkEntry!? op)
;;        empty]
;;       [(RaiseContextExpectedValuesError!? op)
;;        empty]
;;       [(RaiseArityMismatchError!? op)
;;        empty]
;;       [(RaiseOperatorApplicationError!? op)
;;        empty]))
  
;;   (unique/eq?
;;    (let: loop : (Listof Symbol) ([stmts : (Listof Statement) stmts])
;;          (cond [(empty? stmts)
;;                 empty]
;;                [else
;;                 (let: ([stmt : Statement (first stmts)])
;;                   (append (cond
;;                             [(symbol? stmt)
;;                              empty]
;;                             [(LinkedLabel? stmt)
;;                              (list (LinkedLabel-label stmt)
;;                                    (LinkedLabel-linked-to stmt))]
;;                             [(AssignImmediateStatement? stmt)
;;                              (let: ([v : OpArg (AssignImmediateStatement-value stmt)])
;;                                    (collect-input v))]
;;                             [(AssignPrimOpStatement? stmt)
;;                              (collect-primitive-operator (AssignPrimOpStatement-op stmt))]
;;                             [(PerformStatement? stmt)
;;                              (collect-primitive-command (PerformStatement-op stmt))]
;;                             [(TestAndBranchStatement? stmt)
;;                              (list (TestAndBranchStatement-label stmt))]
;;                             [(GotoStatement? stmt)
;;                              (collect-location (GotoStatement-target stmt))]
;;                             [(PushEnvironment? stmt)
;;                              empty]
;;                             [(PopEnvironment? stmt)
;;                              empty]
;;                             [(PushImmediateOntoEnvironment? stmt)
;;                              (collect-input (PushImmediateOntoEnvironment-value stmt))]
;;                             [(PushControlFrame/Generic? stmt)
;;                              empty]
;;                             [(PushControlFrame/Call? stmt)
;;                              (label->labels (PushControlFrame/Call-label stmt))]
;;                             [(PushControlFrame/Prompt? stmt)
;;                              (label->labels (PushControlFrame/Prompt-label stmt))]
;;                             [(PopControlFrame? stmt)
;;                              empty])
;;                           (loop (rest stmts))))]))))


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
      (let*: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)])
             (cond
               [(eq? test 'false?)
                (format "if (~a === false) { ~a }"
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

     [(PushControlFrame/Generic? stmt)
      "MACHINE.control.push(new RUNTIME.Frame());"]
     
     [(PushControlFrame/Call? stmt)
      (format "MACHINE.control.push(new RUNTIME.CallFrame(~a, MACHINE.proc));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Call-label stmt)])
                (cond
                  [(symbol? label) label]
                  [(LinkedLabel? label) (LinkedLabel-label label)])))]

     [(PushControlFrame/Prompt? stmt)
      ;; fixme: use a different frame structure
      (format "MACHINE.control.push(new RUNTIME.PromptFrame(~a, ~a));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Prompt-label stmt)])
                (cond
                  [(symbol? label) label]
                  [(LinkedLabel? label) (LinkedLabel-label label)]))

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


