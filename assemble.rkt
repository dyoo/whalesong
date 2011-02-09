#lang racket/base
(require "structs.rkt"
         racket/string
         racket/list)

(provide (all-defined-out))


(define-struct basic-block (name stmts) #:transparent)
(define (fracture stmts)
  (let loop ([name (make-label 'start)]
             [acc '()]
             [basic-blocks '()]
             [stmts stmts]
             [last-stmt-goto? #f])
    (cond
      [(null? stmts)
       (reverse (cons (make-basic-block name (reverse acc))
                      basic-blocks))]
      [(symbol? (car stmts))
       (loop (car stmts)
             '()
             (cons (make-basic-block name  
                                     (if last-stmt-goto? 
                                         (reverse acc)
                                         (reverse (append `((goto (label ,(car stmts))))
                                                          acc))))
                   basic-blocks)
             (cdr stmts)
             (tagged-list? (car stmts) 'goto))]
      [else
       (loop name
             (cons (car stmts) acc)
             basic-blocks
             (cdr stmts)
             (tagged-list? (car stmts) 'goto))])))


;; assemble-basic-block: basic-block -> string
(define (assemble-basic-block a-basic-block)
  (format "var ~a=function(){\nif(--MACHINE.callsBeforeTrampoline < 0) { throw ~a; }\n~a};"
          (basic-block-name a-basic-block)
          (basic-block-name a-basic-block)
          (string-join (map assemble-stmt (basic-block-stmts a-basic-block))
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
                (assemble-op-expression (op-name (caddr stmt))
                                        (cdddr stmt)))]
       [else
        (error 'assemble "~a" stmt)])]
    [(tagged-list? stmt 'perform)
     (assemble-op-statement (op-name (cadr stmt))
                            (cddr stmt))]
    [(tagged-list? stmt 'test)
     (format "if(~a){"
             (assemble-op-expression (op-name (cadr stmt))
                                     (cddr stmt)))]
    [(tagged-list? stmt 'branch)
     ;; the unbalanced } is deliberate: test and branch always follow each other.
     (format "return ~a();}"
             (assemble-location (cadr stmt)))]
    [(tagged-list? stmt 'goto)
     (format "return ~a();"
             (assemble-location (cadr stmt)))]
    [(tagged-list? stmt 'save)
     (format "MACHINE.stack.push(MACHINE.~a);"
             (cadr stmt))]
    [(tagged-list? stmt 'restore)
     (format "MACHINE.~a=MACHINE.stack.pop();"
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
              (let loop ([assembled-inputs assembled-inputs])
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
       (format "(new ExtendedEnvironment(~a, ~a)"
               (second assembled-inputs)
               (first assembled-inputs))]
      [(lookup-variable-value)
       (format "((~a).globalBindings[~a])"
               (second assembled-inputs)
               (first assembled-inputs))]
      [else
       (error 'assemble "~e" op-name)])))


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