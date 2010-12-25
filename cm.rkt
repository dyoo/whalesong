#lang racket


;; SICP, Chapter 5.5

;; registers: env, argl, proc, val, cont
;; as well as the stack.
(define all-regs '(env argl proc val cont))



;; compile: expression target linkage -> instruction-sequence
(define (compile exp target linkage)
  (cond
    [(self-evaluating? exp)
     (compile-self-evaluating exp target linkage)]
    [(quoted? exp)
     (compile-quoted exp target linkage)]
    [(variable? exp)
     (compile-variable exp target linkage)]
    [(assignment? exp)
     (compile-assignment exp target linkage)]
    [(definition? exp)
     (compile-definition exp target linkage)]
    [(if? exp)
     (compile-if exp target linkage)]
    [(lambda? exp)
     (compile-lambda exp target linkage)]
    [(begin? exp)
     (compile-sequence (begin-actions exp)
                       target
                       linkage)]
    [(application? exp)
     (compile-application exp target linkage)]
    [else
     (error 'compile "Unknown expression type ~e" exp)]))



(define (compile-linkage linkage)
  (cond
    [(eq? linkage 'return)
     (make-instruction-sequence '(cont) '() '((goto (reg cont))))]
    [(eq? linkage 'next)
     empty-instruction-sequence]
    [else
     (make-instruction-sequence '() '()
                                `((goto (label ,linkage))))]))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(cont)
              instruction-sequence
              (compile-linkage linkage)))


(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `((assign ,target (const ,exp))))))


(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `((assign ,target (const ,(text-of-quotation exp)))))))


(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '(env)
                     (list target)
                     `((assign ,target
                               (op lookup-variable-value)
                               (const ,exp)
                               (reg env))))))



(define (compile-assignment exp target linkage)
  (let ([var (assignment-variable exp)]
        [get-value-code
         (compile (assignment-value exp) 'val 'next)])
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val)
                  (list target)
                  `((perform (op set-variable-value!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))


(define (compile-definition exp target linkage)
  (let ([var (definition-variable exp)]
        [get-value-code
         (compile (definition-value exp) 'val 'next)])
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))



(define (compile-if exp target linkage)  
  (let ([t-branch (make-label 'trueBranch)]
        [f-branch (make-label 'falseBranch)]
        [after-if (make-label 'afterIf)])
    (let ([consequent-linkage
           (if (eq? linkage 'next)
               after-if
               linkage)])
      (let ([p-code (compile (if-predicate exp) 'val 'next)]
            [c-code (compile (if-consequent exp) target consequent-linkage)]
            [a-code (compile (if-alternative exp) target linkage)])
        (preserving '(env cont)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val)
                      '()
                      `((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))



(define (compile-sequence seq target linkage) 
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env cont)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))


(define (compile-lambda exp target linkage) 
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
                           `((assign ,target
                                     (op make-compiled-procedure)
                                     (label ,proc-entry)
                                     (reg env)))))
        (compile-lambda-body exp proc-entry))                                                     
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ([formals (lambda-parameters exp)])
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))


(define (compile-application exp target linkage) 
  (let ([proc-code (compile (operator exp) 'proc 'next)]
        [operand-codes (map (lambda (operand)
                              (compile operand 'val 'next))
                            (operands exp))])
    (preserving '(env cont)
                proc-code
                (preserving '(proc cont)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ([operand-codes (reverse operand-codes)])
    (if (null? operand-codes)
        (make-instruction-sequence '()
                                   '(argl) 
                                   '((assign argl (const ()))))
        (let ([code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))])
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ([code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence 
                      '(val argl) 
                      '(argl)
                      '((assign argl (op cons) (reg val) (reg argl)))))])
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))
              

(define (compile-procedure-call target linkage)
  (let ([primitive-branch (make-label 'primitiveBranch)]
        [compiled-branch (make-label 'compiledBranch)]
        [after-call (make-label 'afterCall)])
    (let ([compiled-linkage
           (if (eq? linkage 'next) after-call linkage)])
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
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
                            `((assign ,target
                                      (op apply-primitive-procedure)
                                      (reg proc)
                                      (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond [(and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc) 
          all-regs
          `((assign cont (label ,linkage))
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val))))]
        [(and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ([proc-return (make-label 'procReturn)])
           (make-instruction-sequence
            '(proc)
            all-regs
            `((assign cont (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage)))))]
        [(and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence
          '(proc cont)
          all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val))))]
        [(and (not (eq? target 'val))
              (eq? linkage 'return))
         (error 'compile "return linkage, target not val: ~s" target)]))
                   
            
             





;; instruction sequences
(define-struct instruction-sequence (needs modifies statements) #:transparent)

(define empty-instruction-sequence (make-instruction-sequence '() '() '()))

(define (make-label l)
  (gensym l))


(define (registers-needed s)
  (if (symbol? s) '() (instruction-sequence-needs s)))
(define (registers-modified s)
  (if (symbol? s) '() (instruction-sequence-modifies s)))
(define (statements s)
  (if (symbol? s) (list s) (instruction-sequence-statements s)))


(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


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
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))
            
  
  
  
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        empty-instruction-sequence
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond [(null? s1) s2]
        [(memq (car s1) s2)
         (list-union (cdr s1) s2)]
        [else (cons (car s1) (list-union (cdr s1) s2))]))

(define (list-difference s1 s2)
  (cond [(null? s1) '()]
        [(memq (car s1) s2)
         (list-difference (cdr s1) s2)]
        [else
         (cons (car s1) (list-difference (cdr s1) s2))]))



(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
                             (registers-modified seq)
                             (append (statements seq) (statements body-seq))))


(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))










;; expression selectors

(define (self-evaluating? exp)
  (cond
    [(number? exp) #t]
    [(string? exp) #t]
    [else #f]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))





(define-struct chunk (name stmts) #:transparent)
(define (fracture stmts)
  (let loop ([name (make-label 'start)]
             [acc '()]
             [chunks '()]
             [stmts stmts]
             [last-stmt-goto? #f])
    (cond
      [(null? stmts)
       (reverse (cons (make-chunk name (reverse acc))
                      chunks))]
      [(symbol? (car stmts))
       (loop (car stmts)
             '()
             (cons (make-chunk name  
                               (if last-stmt-goto? 
                                   (reverse acc)
                                   (reverse (append `((goto (label ,(car stmts))))
                                                    acc))))
                   chunks)
             (cdr stmts)
             (tagged-list? (car stmts) 'goto))]
      [else
       (loop name
             (cons (car stmts) acc)
             chunks
             (cdr stmts)
             (tagged-list? (car stmts) 'goto))])))


;; assemble-chunk: chunk -> string
(define (assemble-chunk a-chunk)
  (format "var ~a=function(){~a};"
          (chunk-name a-chunk)
          (string-join (map assemble-stmt (chunk-stmts a-chunk))
                       "\n")))

(define (location? stmt)
  (or (tagged-list? stmt 'reg)
      (tagged-list? stmt 'label)))

(define (const? stmt)
  (tagged-list? stmt 'const))

(define (reg? s)
  (tagged-list? s 'reg))

(define (label? s)
  (tagged-list? s 'label))

(define (op? s)
  (tagged-list? s 'op))

(define (op-name s)
  (cadr s))

;; assemble-stmt: stmt -> string
(define (assemble-stmt stmt)
  (cond
    [(tagged-list? stmt 'assign)
     (cond 
       [(reg? (caddr stmt))
        (format "MACHINE.~a=~a" 
                (cadr stmt)
                (assemble-reg (caddr stmt)))]
       [(label? (caddr stmt))
        (format "MACHINE.~a=~a;" (cadr stmt) 
                (assemble-label (caddr stmt)))]
       [(const? (caddr stmt))
        (format "MACHINE.~a=~a;" 
                (cadr stmt)
                (assemble-const (caddr stmt)))]
       [(op? (caddr stmt))
        (format "MACHINE.~a=~a;" 
                (cadr stmt)
                (assemble-op-call (op-name (caddr stmt))
                                  (cdddr stmt)))]
       [else
        (error 'assemble "~a" stmt)])]
    [(tagged-list? stmt 'perform)
     (assemble-op-call (op-name (cadr stmt))
                       (cddr stmt))]
    [(tagged-list? stmt 'test)
     (format "if(~a){"
             (assemble-op-call (op-name (cadr stmt))
                               (cddr stmt)))]
    [(tagged-list? stmt 'branch)
     (format "if(--MACHINE.gas){return ~a()}else{throw ~a}}"
             (assemble-location (cadr stmt))
             (assemble-location (cadr stmt)))]
    [(tagged-list? stmt 'goto)
     (format "if(--MACHINE.gas){return ~a()}else{throw ~a}"
             (assemble-location (cadr stmt))
             (assemble-location (cadr stmt)))]
    [(tagged-list? stmt 'save)
     (format "MACHINE.push(MACHINE.~a);"
             (cadr stmt))]
    [(tagged-list? stmt 'restore)
     (format "MACHINE.~a=MACHINE.pop();"
             (cadr stmt))]
    [else (error 'assemble "~a" stmt)]))

;; fixme: use js->string
(define (assemble-const stmt)
  (let loop ([val (cadr stmt)])
    (cond [(symbol? val)
           (format "~s" (symbol->string val))]
          [(list? val)
           (format "_list(~a)" (string-join (map loop val)
                                            ","))]
          [else
           (format "~s" val)])))

(define (assemble-op-call op-name inputs)
  (format "~a(~a)"
          (case op-name
            [(lookup-variable-value) "_envLookup"]
            [(set-variable-value!) "_envSet"]
            [(define-variable!) "_envDefine"]
            [(false?) "_isFalse"]
            [(make-compiled-procedure) "_makeClosure"]
            [(compiled-procedure-env) "_closureEnv"]
            [(compiled-procedure-entry) "_closureEntry"]
            [(extend-environment) "_envExtend"]
            [(list) "_list"]
            [(cons) "_cons"]
            [(primitive-procedure?) "_isPrimProc"]
            [(apply-primitive-procedure) "_applyPrimProc"]
            [else (error 'assemble "~e" op-name)])
          (string-join (map assemble-input inputs) ",")))

(define (assemble-input an-input)
  (cond
    [(reg? an-input)
     (assemble-reg an-input)]
    [(const? an-input)
     (assemble-const an-input)]
    [(label? an-input)
     (assemble-label an-input)]
    [else (error 'assemble-input "~e" an-input)]))

(define (assemble-location a-location)
  (cond
    [(reg? a-location)
     (assemble-reg a-location)]
    [(label? a-location)
     (assemble-label a-location)]
    [else (error 'assemble-location "~e" a-location)]))

(define (assemble-reg a-reg)
  (string-append "MACHINE." (symbol->string (cadr a-reg))))

(define (assemble-label a-label)
  (symbol->string (cadr a-label)))

 



(define (test)
  (for-each (lambda (chunk)
              (displayln (assemble-chunk chunk)))
            (fracture (statements (compile '(define (factorial n)
                                              (if (= n 0)
                                                  1
                                                  (* (factorial (- n 1))
                                                     n)))
                                           'val
                                           'next)))))