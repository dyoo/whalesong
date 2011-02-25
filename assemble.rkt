#lang typed/racket/base
(require "typed-structs.rkt"
         "helpers.rkt"
         racket/string
         racket/list)

(provide (all-defined-out))


;; assemble/write-invoke: (listof statement) output-port -> void
(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
(define (assemble/write-invoke stmts op)
  (let ([basic-blocks (fracture stmts)])
    (fprintf op "function(success, fail, params) {\n")
    (fprintf op "var param;\n")
    (for-each (lambda: ([basic-block : BasicBlock])
                (displayln (assemble-basic-block basic-block) op)
                (newline op))
              basic-blocks)
    (fprintf op "MACHINE.cont = function() {success(MACHINE.val)};\n")
    (fprintf op "MACHINE.params.currentErrorHandler = function(e) { fail(e); };\n")
    (fprintf op #<<EOF
for (param in params) {
    if (params.hasOwnProperty(param)) {
        MACHINE.params[param] = params[param];
    }
}
EOF
             )
    (fprintf op "trampoline(~a, function() {}, function(e) { MACHINE.params.currentErrorHandler(e)}); }"
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
       (list (Label-name an-input))]))

  (: collect-location ((U Reg Label) -> (Listof Symbol)))
  (define (collect-location a-location)
    (cond
      [(Reg? a-location)
       empty]
      [(Label? a-location)
       (list (Label-name a-location))]))
  
  (unique
   (let loop ([stmts stmts])
     (cond [(empty? stmts)
            empty]
           [else
            (let ([stmt (first stmts)])
              (append (cond
                        [(symbol? stmt)
                         empty]
                        [(AssignImmediateStatement? stmt)
                         (let ([v (AssignImmediateStatement-value stmt)])
                           (cond 
                             [(Reg? v)
                              empty]
                             [(Label? v)
                              (list (Label-name v))]
                             [(Const? v)
                              empty]))]
                        [(AssignPrimOpStatement? stmt)
                         (apply append (map collect-input (AssignPrimOpStatement-rands stmt)))]
                        [(PerformStatement? stmt)
                         (apply append (map collect-input (PerformStatement-rands stmt)))]
                        [(TestStatement? stmt)
                         empty]
                        [(BranchLabelStatement? stmt)
                         (list (BranchLabelStatement-label stmt))]
                        [(GotoStatement? stmt)
                         (collect-location (GotoStatement-target stmt))]
                        [(SaveStatement? stmt)
                         empty]
                        [(RestoreStatement? stmt)
                         empty])
                      (loop (rest stmts))))]))))




;; assemble-basic-block: basic-block -> string
(: assemble-basic-block (BasicBlock -> String))
(define (assemble-basic-block a-basic-block)
  (format "var ~a=function(){\nif(--MACHINE.callsBeforeTrampoline < 0) { throw ~a; }\n~a};"
          (BasicBlock-name a-basic-block)
          (BasicBlock-name a-basic-block)
          (string-join (map assemble-stmt (BasicBlock-stmts a-basic-block))
                       "\n")))


;; assemble-stmt: stmt -> string
(: assemble-stmt (UnlabeledStatement -> String))
(define (assemble-stmt stmt)
  (cond
    [(AssignImmediateStatement? stmt)
     (let ([v (AssignImmediateStatement-value stmt)])
       (cond 
         [(Reg? v)
          (format "MACHINE.~a=~a" 
                  (AssignImmediateStatement-target stmt)
                  (assemble-reg v))]
         [(Label? v)
          (format "MACHINE.~a=~a;" 
                  (AssignImmediateStatement-target stmt)
                  (assemble-label v))]
         [(Const? v)
          (format "MACHINE.~a=~a;" 
                  (AssignImmediateStatement-target stmt)
                  (assemble-const v))]))]
    
    [(AssignPrimOpStatement? stmt)
     (format "MACHINE.~a=~a;" 
             (AssignPrimOpStatement-target stmt)
             (assemble-op-expression (AssignPrimOpStatement-op stmt)
                                     (AssignPrimOpStatement-rands stmt)))]
    [(PerformStatement? stmt)
     (assemble-op-statement (PerformStatement-op stmt)
                            (PerformStatement-rands stmt))]
    [(TestStatement? stmt)
     (format "if(~a){"
             (assemble-op-expression (TestStatement-op stmt)
                                     (list (make-Reg (TestStatement-register-rand stmt)))))]
    [(BranchLabelStatement? stmt)
     ;; the unbalanced } is deliberate: test and branch always follow each other.
     (format "return ~a();}"
             (assemble-location (make-Label (BranchLabelStatement-label stmt))))]
    [(GotoStatement? stmt)
     (format "return ~a();"
             (assemble-location (GotoStatement-target stmt)))]
    [(SaveStatement? stmt)
     (format "MACHINE.stack.push(MACHINE.~a);"
             (SaveStatement-reg stmt))]
    [(RestoreStatement? stmt)
     (format "MACHINE.~a=MACHINE.stack.pop();"
             (RestoreStatement-reg stmt))]))
    

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


(: assemble-op-expression ((U PrimitiveOperator TestOperator) (Listof OpArg) -> String))
(define (assemble-op-expression op-name inputs)
  (let ([assembled-inputs (map assemble-input inputs)])
    (case op-name
      ;; open coding some of the primitive operations:
      [(compiled-procedure-entry)
       (format "(~a.label)" (assemble-input (first inputs)))]
      [(compiled-procedure-env)
       (format "(~a.env)" (assemble-input (first inputs)))]
      [(make-compiled-procedure)
       (format "(new Closure(~a, ~a))"
               (second assembled-inputs)
               (first assembled-inputs))]
      [(false?)
       (format "(!(~a))" (assemble-input (first inputs)))]
      [(cons)
       (format "[~a]" (string-join (map assemble-input inputs) ","))]
      [(list)
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
      [(apply-primitive-procedure)
       (format "~a(~a)" 
               (first assembled-inputs)
               (second assembled-inputs))]
      [(lexical-address-lookup)
       (format "(~a).valss[~a][~a]"
               (third assembled-inputs)
               (first assembled-inputs)
               (second assembled-inputs))]
      [(toplevel-lookup)
       (let ([depth (first assembled-inputs)]
             [pos (second assembled-inputs)]
             [name (third assembled-inputs)]
             [env (fourth assembled-inputs)])
         (format "(~a).valss[~a][~a]" env depth pos))]
      [(primitive-procedure?)
       (format "(typeof(~a) === 'function')"
               (first assembled-inputs))]
      [(extend-environment)
       (format "new ExtendedEnvironment(~a, ~a)"
               (second assembled-inputs)
               (first assembled-inputs))]
      [(extend-environment/prefix)
       (format "new ExtendedPrefixEnvironment(~a, ~a)"
               (second assembled-inputs)
               (first assembled-inputs))])))


(: assemble-op-statement (PerformOperator (Listof OpArg) -> String))
(define (assemble-op-statement op-name inputs)
  (let ([assembled-inputs (map assemble-input inputs)])
    (case op-name
      [(lexical-address-set!)
       (format "(~a).valss[~a][~a] = ~a;"
               (third assembled-inputs)
               (first assembled-inputs)
               (second assembled-inputs)
               (fourth assembled-inputs))]
      [(toplevel-set!)
       (let ([depth (first assembled-inputs)]
             [pos (second assembled-inputs)]
             [name (third assembled-inputs)]
             [env (fourth assembled-inputs)]
             [val (fifth assembled-inputs)])
         (format "(~a).valss[~a][~a] = ~a;"
                 env
                 depth
                 pos
                 val))]
      [(check-bound!)
       (let ([depth (first assembled-inputs)]
             [pos (second assembled-inputs)]
             [name (third assembled-inputs)]
             [env (fourth assembled-inputs)])
         (format "if ((~a).valss[~a][~a] === undefined) { throw new Error(\"Not bound: \" + ~a); }"
                 env
                 depth
                 pos
                 name))])))




(: assemble-input ((U Reg Const Label) -> String))
(define (assemble-input an-input)
  (cond
    [(Reg? an-input)
     (assemble-reg an-input)]
    [(Const? an-input)
     (assemble-const an-input)]
    [(Label? an-input)
     (assemble-label an-input)]))
    
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