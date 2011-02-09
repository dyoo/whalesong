#lang racket/base
(require "structs.rkt"
         racket/string
         racket/list)

(provide (all-defined-out))



(define-struct basic-block (name stmts) #:transparent)
(define (fracture stmts)
  (let* ([first-block-label (make-label 'start)]
         [headers (cons first-block-label (collect-headers stmts))])
    (let loop ([name first-block-label]
               [acc '()]
               [basic-blocks '()]
               [stmts stmts]
               [last-stmt-goto? #f])
      (cond
        [(null? stmts)
         (reverse (cons (make-basic-block name (reverse acc))
                        basic-blocks))]
        [(symbol? (car stmts))
         (cond
           [(member (car stmts) headers)
            (loop (car stmts)
                  '()
                  (cons (make-basic-block name  
                                          (if last-stmt-goto? 
                                              (reverse acc)
                                              (reverse (append `((goto (label ,(car stmts))))
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
               (tagged-list? (car stmts) 'goto))]))))


;; unique: (listof symbol -> listof symbol)
(define (unique los)
  (let ([ht (make-hasheq)])
    (for ([l los])
      (hash-set! ht l #t))
    (for/list ([k (in-hash-keys ht)]) 
      k)))


;; collect-headers: (listof stmt) -> (listof label)
;; collects all the labels that are potential targets for GOTOs or branches.
(define (collect-headers stmts)
  (define (collect-input an-input)
    (cond
      [(reg? an-input)
       empty]
      [(const? an-input)
       empty]
      [(label? an-input)
       (list (label-name an-input))]
      [else (error 'collect-input "~e" an-input)]))
  (define (collect-location a-location)
    (cond
      [(reg? a-location)
       empty]
      [(label? a-location)
       (list (label-name a-location))]
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
                        [(tagged-list? stmt 'assign)
                         (cond 
                           [(reg? (caddr stmt))
                            empty]
                           [(label? (caddr stmt))
                            (list (label-name (caddr stmt)))]
                           [(const? (caddr stmt))
                            empty]
                           [(op? (caddr stmt))
                            (apply append (map collect-input (cdddr stmt)))]
                           [else
                            (error 'assemble "~a" stmt)])]
                        [(tagged-list? stmt 'perform)
                         (apply append (map collect-input (cddr stmt)))]
                        [(tagged-list? stmt 'test)
                         (apply append (map collect-input (cddr stmt)))]
                        [(tagged-list? stmt 'branch)
                         (collect-location (cadr stmt))]
                        [(tagged-list? stmt 'goto)
                         (collect-location (cadr stmt))]
                        [(tagged-list? stmt 'save)
                         empty]
                        [(tagged-list? stmt 'restore)
                         empty]
                        [else
                         (error 'assemble "~a" stmt)])
                      (loop (rest stmts))))]))))



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

(define (label-name a-label)
  (cadr a-label))


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
  (symbol->string (label-name a-label)))