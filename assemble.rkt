#lang typed/racket/base
(require "typed-structs.rkt"
         racket/string
         racket/list)

(provide (all-defined-out))


;; assemble/write-invoke: (listof statement) output-port -> void
(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
(define (assemble/write-invoke stmts op)
  (let ([basic-blocks (fracture stmts)])
    (fprintf op "function(k) {\n")
    (for-each (lambda: ([basic-block : BasicBlock])
                (displayln (assemble-basic-block basic-block) op)
                (newline op))
              basic-blocks)
    (fprintf op "MACHINE.cont = k;\n")
    (fprintf op "trampoline(~a, function() {}); }"
             (BasicBlock-name (first basic-blocks)))))




;; fracture: (listof stmt) -> (listof basic-block)
(: fracture ((Listof Statement) -> (Listof BasicBlock)))
(define (fracture stmts)
  (let* ([first-block-label (make-label 'start)]
         [jump-targets 
          (cons first-block-label (collect-general-jump-targets stmts))])
    (let: loop : (Listof BasicBlock)
          ([name : Symbol first-block-label]
           [acc : (Listof Statement) '()]
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



;; unique: (listof symbol -> listof symbol)
(: unique ((Listof Symbol) -> (Listof Symbol)))
(define (unique los)
  (let: ([ht : (HashTable Symbol Boolean) (make-hasheq)])
    (for ([l los])
      (hash-set! ht l #t))
    (hash-map ht (lambda: ([k : Symbol] [v : Boolean]) k))))



;; collect-general-jump-targets: (listof stmt) -> (listof label)
;; collects all the labels that are potential targets for GOTOs or branches.
(: collect-general-jump-targets ((Listof Statement) -> (Listof Symbol)))
(define (collect-general-jump-targets stmts)
  (define (collect-input an-input)
    (cond
      [(Reg? an-input)
       empty]
      [(Const? an-input)
       empty]
      [(Label? an-input)
       (list (Label-name an-input))]
      [else (error 'collect-input "~e" an-input)]))
  (define (collect-location a-location)
    (cond
      [(Reg? a-location)
       empty]
      [(Label? a-location)
       (list (Label-name a-location))]
      [else (error 'collect-location "~e" a-location)]))
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



;; collect-indirect-jump-targets: (listof stmt) -> (listof label)
;; collects the labels that are potential targets for GOTOs or branches from
;; indirect jumps.
;; The only interesting case should be where there's a register assignment
;; whose value is a label.
#;(: collect-indirect-jump-targets ((Listof Statement) -> (Listof Symbol)))
#;(define (collect-indirect-jump-targets stmts)
  (define (collect-input an-input)
    (cond
      [(reg? an-input)
       empty]
      [(const? an-input)
       empty]
      [(label? an-input)
       empty]
      [else (error 'collect-input "~e" an-input)]))
  (define (collect-location a-location)
    (cond
      [(reg? a-location)
       empty]
      [(label? a-location)
       empty]
      [else 
       (error 'collect-location "~e" a-location)]))
  (unique
   (let loop ([stmts stmts])
     (cond [(empty? stmts)
            empty]
           [else
            (let ([stmt (first stmts)])
              (append (cond
                        [(symbol? stmt)
                         empty]
                        [(tagged-list? stmt 'assign)
                         (cond 
                           [(reg? (caddr stmt))
                            empty]
                           [(label? (caddr stmt))
                            ;; Watch assignments of labels into registers.
                            (list (label-name (caddr stmt)))]
                           [(const? (caddr stmt))
                            empty]
                           [(op? (caddr stmt))
                            empty]
                           [else
                            (error 'assemble "~a" stmt)])]
                        [(tagged-list? stmt 'perform)
                         empty]
                        [(tagged-list? stmt 'test)
                         empty]
                        [(tagged-list? stmt 'branch)
                         empty]
                        [(tagged-list? stmt 'goto)
                         empty]
                        [(tagged-list? stmt 'save)
                         empty]
                        [(tagged-list? stmt 'restore)
                         empty]
                        [else
                         (error 'assemble "~a" stmt)])
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
(: assemble-stmt (Statement -> String))
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
              [(list? val)
               (format "_list(~a)" (string-join (map loop val)
                                                ","))]
              [else
               (format "~s" val)])))

(: assemble-op-expression (Symbol (Listof OpArg) -> String))
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
      [(primitive-procedure?)
       (format "(typeof(~a) === 'function')"
               (first assembled-inputs))]
      [(extend-environment)
       (format "new ExtendedEnvironment(~a, ~a)"
               (second assembled-inputs)
               (first assembled-inputs))]
      [(lookup-variable-value)
       (format "((~a).globalBindings[~a])"
               (second assembled-inputs)
               (first assembled-inputs))]
      [else
       (error 'assemble "~e" op-name)])))

(: assemble-op-statement (Symbol (Listof OpArg) -> String))
(define (assemble-op-statement op-name inputs)
  (let ([assembled-inputs (map assemble-input inputs)])
    (case op-name
      [(define-variable!)
       (format "(~a).globalBindings[~a] = ~a;"
               (third assembled-inputs)
               (first assembled-inputs)
               (second assembled-inputs))]
      [(set-variable-value!)
       (format "(~a).globalBindings[~a] = ~a;"
               (third assembled-inputs)
               (first assembled-inputs)
               (second assembled-inputs))]
      [(lexical-address-set!)
       (format "(~a).valss[~a][~a] = ~a;"
               (third assembled-inputs)
               (first assembled-inputs)
               (second assembled-inputs)
               (fourth assembled-inputs))]
      [(check-bound-global!)
       (format "if (! (~a).globalBindings.hasOwnProperty(~a)) { throw new Error(\"Not bound: \" + ~a); }"
               (second assembled-inputs)
               (first assembled-inputs)
               (first assembled-inputs))]
      [else
       (error 'assemble-op-statement "~a" op-name)])))



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