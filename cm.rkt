#lang racket/base

(require "structs.rkt"
         "assemble.rkt"
         racket/list)

;; SICP, Chapter 5.5

;; registers: env, argl, proc, val, cont
;; as well as the stack.
(define all-regs '(env argl proc val cont))



;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.


;; find-variable: symbol compile-time-environment -> lexical-address
;; Find where the variable should be located.
(define (find-variable name cenv)
  (define (find-pos sym los)
    (cond
      [(eq? sym (car los))
       0]
      [else
       (add1 (find-pos sym (cdr los)))]))
  (let loop ([cenv cenv]
             [depth 0])
    (cond [(empty? cenv)
           'not-found]
          [(member name (first cenv))
           (list depth (find-pos name (first cenv)))]
          [else
           (loop (rest cenv) (add1 depth))])))

;; global-lexical-address?: lexical-address -> boolean
;; Produces true if the address refers to the toplevel environment.
(define (global-lexical-address? address)
  (eq? address 'not-found))


;; extend-lexical-environment: lexical-environment (listof symbol) -> lexical-envrionment
(define (extend-lexical-environment cenv names)
  (cons names cenv))




;; compile: expression target linkage -> instruction-sequence
(define (compile exp cenv target linkage)
  (cond
    [(self-evaluating? exp)
     (compile-self-evaluating exp cenv target linkage)]
    [(quoted? exp)
     (compile-quoted exp cenv target linkage)]
    [(variable? exp)
     (compile-variable exp cenv target linkage)]
    [(assignment? exp)
     (compile-assignment exp cenv target linkage)]
    [(definition? exp)
     (compile-definition exp cenv target linkage)]
    [(if? exp)
     (compile-if exp cenv target linkage)]
    [(lambda? exp)
     (compile-lambda exp cenv target linkage)]
    [(begin? exp)
     (compile-sequence (begin-actions exp)
                       cenv
                       target
                       linkage)]
    [(application? exp)
     (compile-application exp cenv target linkage)]
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


(define (compile-self-evaluating exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `((assign ,target (const ,exp))))))


(define (compile-quoted exp cenv target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '()
                     (list target)
                     `((assign ,target (const ,(text-of-quotation exp)))))))


(define (compile-variable exp cenv target linkage)
  (let ([lexical-pos (find-variable exp cenv)])
    (cond
      [(global-lexical-address? lexical-pos)
       (end-with-linkage linkage
                         (make-instruction-sequence
                          '(env)
                          (list target)
                          ;; Slight modification: explicitly testing for
                          ;; global variable binding before lookup.
                          `((perform (op check-bound-global!)
                                     (const ,exp)
                                     (reg env))
                            (assign ,target
                                    (op lookup-variable-value)
                                    (const ,exp)
                                    (reg env)))))]
      [else
       (end-with-linkage linkage
                         (make-instruction-sequence
                          '(env)
                          (list target)
                          `((assign ,target
                                    (op lexical-address-lookup)
                                    (const ,(first lexical-pos))
                                    (const ,(second lexical-pos))
                                    (reg env)))))])))



(define (compile-assignment exp cenv target linkage)
  (let* ([var (assignment-variable exp)]
         [get-value-code
          (compile (assignment-value exp) cenv 'val 'next)]
         [lexical-address
          (find-variable var cenv)])
    (cond
      [(global-lexical-address? lexical-address)
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
                       (assign ,target (const ok))))))]
      [else
       (end-with-linkage
        linkage
        (preserving '(env)
                    get-value-code
                    (make-instruction-sequence
                     '(env val)
                     (list target)
                     `((perform (op lexical-address-set!)
                                (const ,(first lexical-address))
                                (const ,(second lexical-address))
                                (reg env)
                                (reg val))
                       (assign ,target (const ok))))))])))


;; FIXME: exercise 5.43
(define (compile-definition exp cenv target linkage)
  (let ([var (definition-variable exp)]
        [get-value-code
         (compile (definition-value exp) cenv 'val 'next)])
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



(define (compile-if exp cenv target linkage)  
  (let ([t-branch (make-label 'trueBranch)]
        [f-branch (make-label 'falseBranch)]
        [after-if (make-label 'afterIf)])
    (let ([consequent-linkage
           (if (eq? linkage 'next)
               after-if
               linkage)])
      (let ([p-code (compile (if-predicate exp) cenv 'val 'next)]
            [c-code (compile (if-consequent exp) cenv target consequent-linkage)]
            [a-code (compile (if-alternative exp) cenv target linkage)])
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



(define (compile-sequence seq cenv target linkage) 
  (if (last-exp? seq)
      (compile (first-exp seq) cenv target linkage)
      (preserving '(env cont)
                  (compile (first-exp seq) cenv target 'next)
                  (compile-sequence (rest-exps seq) cenv target linkage))))


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
                           `((assign ,target
                                     (op make-compiled-procedure)
                                     (label ,proc-entry)
                                     ;; TODO: rather than capture the whole
                                     ;; environment, we may instead
                                     ;; just capture the free variables.
                                     ;; But that requires that we box
                                     ;; up all set!-ed variables, in order
                                     ;; to preserve semantics of set!
                                     (reg env)))))
        (compile-lambda-body exp cenv
                             proc-entry))
       after-lambda))))


(define (compile-lambda-body exp cenv proc-entry)
  (let* ([formals (lambda-parameters exp)]
         [extended-cenv (extend-lexical-environment cenv formals)])
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) extended-cenv 'val 'return))))


(define (compile-application exp cenv target linkage) 
  (let ([proc-code (compile (operator exp) cenv 'proc 'next)]
        [operand-codes (map (lambda (operand)
                              (compile operand cenv 'val 'next))
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

















(define (test source-code)
  (let ([basic-blocks
         (fracture (statements (compile source-code
                                        '()
                                        'val
                                        'return)))])
    (printf "var invoke = function(MACHINE, k) {\n")
    (for-each (lambda (basic-block)
                (displayln (assemble-basic-block basic-block))
                (newline))
              basic-blocks)
    (printf "MACHINE.cont = k;\n")
    (printf "trampoline(~a, function() {}); };\n"
            (basic-block-name (first basic-blocks)))))


#;(test '(define (factorial n)
           (if (= n 0)
               1
               (* (factorial (- n 1))
                  n))))
#;(test '(define (gauss n)
         (if (= n 0)
             0
             (+ (gauss (- n 1))
                n))))

(test '(define (fib n)
           (if (< n 2)
               1
               (+ (fib (- n 1))
                  (fib (- n 2))))))