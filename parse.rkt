#lang racket/base

(require "expression-structs.rkt"
         "lexical-env.rkt"
         "lexical-structs.rkt"
         "helpers.rkt"
         "parameters.rkt"
         racket/list)

(provide (rename-out (-parse parse))
         
         ;; meant for tests
         set-private-lam-label-counter!)

(define (-parse exp)
  (let* ([prefix (construct-the-prefix exp)])
    (make-Top prefix (parse exp (extend-lexical-environment '() prefix) #t))))



(define (construct-the-prefix exp)
  (let ([unbound-names (find-unbound-names exp)]
        [mutated-names (find-mutated-names exp)])
    (make-Prefix (map (lambda (s)
                        (cond
                          [(member s mutated-names)
                           s]
                          [(lookup-in-current-language s)
                           =>
                           (lambda (mv) mv)]
                          [else
                           s]))
                      unbound-names))))



;; a language maps identifiers to module variables.
(define current-language 
  (make-parameter '(display newline displayln pi e
                            = < > <= >= + * - / cons
                            list car cdr pair? set-car!
                            set-cdr! not null null?
                            add1 sub1 zero? vector
                            vector->list list->vector
                            vector-ref vector-set! symbol?
                            symbol->string string-append
                            string-length box unbox set-box!
                            void eq? equal?)))
;; lookup-in-current-language: symbol -> (or ModuleVariable #f)
(define (lookup-in-current-language sym)
  (cond
    [(current-language)
     => (lambda (lang)
          (if (member sym lang)
              (make-ModuleVariable sym '#%kernel)
              #f))]
    [else
     #f]))



;; find-prefix: ParseTimeEnvironment -> Natural
(define (find-prefix cenv)
  (cond
    [(empty? cenv)
     (error 'impossible)]
    [(Prefix? (first cenv))
     0]
    [else
     (add1 (find-prefix (rest cenv)))]))


;; parse: Any ParseTimeEnvironment -> Expression
;; Compile an expression.
(define (parse exp cenv at-toplevel?)
  (cond
    [(self-evaluating? exp)
     (make-Constant exp)]
    
    [(quoted? exp)
     (make-Constant (text-of-quotation exp))]
    
    [(variable? exp)
     (let ([address (find-variable exp cenv)])
       (cond
         [(EnvLexicalReference? address)
          (make-LocalRef (EnvLexicalReference-depth address)
                         (EnvLexicalReference-unbox? address))]
         [(EnvPrefixReference? address)
          (make-ToplevelRef (EnvPrefixReference-depth address)
                            (EnvPrefixReference-pos address))]))]
    
    [(definition? exp)
     (let ([address (find-variable (definition-variable exp) cenv)])
       (cond
         [(EnvLexicalReference? address)
          (error 'parse "Can't define except in toplevel context")]
         [(EnvPrefixReference? address)
          (make-ToplevelSet (EnvPrefixReference-depth address)
                            (EnvPrefixReference-pos address)
                            (definition-variable exp)
                            (parameterize ([current-defined-name (definition-variable exp)])
                              (parse (definition-value exp) cenv #f)))]))]
    
    [(if? exp)
     (make-Branch (parse (if-predicate exp) cenv #f)
                  (parse (if-consequent exp) cenv #f)
                  (parse (if-alternative exp) cenv #f))]
    
    [(cond? exp)
     (parse (desugar-cond exp) cenv #f)]
    
    [(lambda? exp)
     (parse-lambda exp cenv)]
    
    [(begin? exp)
     (let ([actions (map (lambda (e)
                           (parse e cenv at-toplevel?))
                         (begin-actions exp))])
       ((if at-toplevel? make-Splice seq) actions))]
    
    [(named-let? exp)
     (parse (desugar-named-let exp) cenv #f)]
    
    [(let*? exp)
     (parse (desugar-let* exp) cenv #f)]
    
    [(let? exp)
     (parse-let exp cenv)]
    
    [(letrec? exp)
     (parse-letrec exp cenv)]
    
    [(set!? exp)
     (let ([address (find-variable (set!-name exp) cenv)])
       ;; Subtle: this needs to be a sequence here to disable tail calls for the
       ;; extent of the set!-value.
       (make-Seq (list (cond
                         [(EnvLexicalReference? address)
                          (make-InstallValue (EnvLexicalReference-depth address)
                                             (parse (set!-value exp) cenv #f)
                                             #t)]
                         [(EnvPrefixReference? address)
                          (make-ToplevelSet (EnvPrefixReference-depth address)
                                            (EnvPrefixReference-pos address)
                                            (definition-variable exp)
                                            (parse (set!-value exp) cenv #f))])
                       (make-Constant (void)))))]

    ;; Remember, this needs to be the last case.
    [(application? exp)
     (let ([cenv-with-scratch-space
            (extend-lexical-environment/placeholders cenv (length (operands exp)))])
       (make-App (parse (operator exp) cenv-with-scratch-space #f)
                 (map (lambda (rand) (parse rand cenv-with-scratch-space #f))
                      (operands exp))))]
    [else
     (error 'compile "Unknown expression type ~e" exp)]))



(define (parse-lambda exp cenv)
  (let* ([unbound-names (find-unbound-names exp)]
         [mutated-parameters (list-intersection (find-mutated-names `(begin ,@(lambda-body exp)))
                                                (lambda-parameters exp))]
         [closure-references (collect-lexical-references
                              (map (lambda (var)
                                     (find-variable var cenv))
                                   unbound-names))]
         [body-cenv (lexical-references->compile-time-environment
                     closure-references
                     cenv
                     (extend-lexical-environment/parameter-names '() 
                                                                 (lambda-parameters exp)
                                                                 (map (lambda (p)
                                                                        (and (member p mutated-parameters) #t))
                                                                      (lambda-parameters exp)))
                     unbound-names)])
    (let ([lam-body (foldl (lambda (a-mutated-param code)
                             (make-BoxEnv (env-reference-depth (find-variable a-mutated-param body-cenv))
                                          code))
                           (seq (map (lambda (b)
                                       (parse b body-cenv #f))
                                     (lambda-body exp)))
                           mutated-parameters)])
      (cond [(lambda-has-rest-parameter? exp)
             (make-Lam (current-defined-name)
                       (sub1 (length (lambda-parameters exp)))
                       #t
                       lam-body
                       (map env-reference-depth closure-references)
                       (fresh-lam-label))]
            [else
             (make-Lam (current-defined-name)
                       (length (lambda-parameters exp))
                       #f
                       lam-body
                       (map env-reference-depth closure-references)
                       (fresh-lam-label))]))))


(define lam-label-counter 0)
(define (set-private-lam-label-counter! x)
  (set! lam-label-counter x))
(define fresh-lam-label
    (lambda ()
      (set! lam-label-counter (add1 lam-label-counter))
      (string->symbol (format "lamEntry~a" lam-label-counter))))


(define (seq codes)
  (cond
    [(= 1 (length codes))
     (first codes)]
    [else
     (make-Seq codes)]))




;; find-unbound-names: Any -> (Listof Symbol)
;; Fixme: Cache this.
(define (find-unbound-names exp)
  (unique/eq?
   (let loop ([exp exp])
     (cond
       [(self-evaluating? exp)
        '()]
       
       [(quoted? exp)
        '()]
       
       [(variable? exp)
        (list exp)]
       
       [(definition? exp)
        (cons (definition-variable exp)
              (loop (definition-value exp)))]
       
       [(if? exp)
        (append (loop (if-predicate exp))
                (loop (if-consequent exp))
                (loop (if-alternative exp)))]
       
       [(cond? exp)
        (loop (desugar-cond exp))]
       
       [(lambda? exp)
        (list-difference (apply append (map loop (lambda-body exp)))
                         (lambda-parameters exp))]
       
       [(begin? exp)
        (apply append (map loop (begin-actions exp)))]
       
       [(named-let? exp)
        (loop (desugar-named-let exp))]
       
       [(let*? exp)
        (loop (desugar-let* exp))]
       
       [(let? exp)
        (append (apply append (map loop (let-rhss exp)))
                (list-difference (apply append (map loop (let-body exp)))
                                 (let-variables exp)))]
       
       [(letrec? exp)
        (list-difference (append (apply append (map loop (let-rhss exp)))
                                 (apply append (map loop (let-body exp))))
                         (let-variables exp))]
       
       [(set!? exp)
        (cons (set!-name exp)
              (loop (set!-value exp)))]

       ;; Remember: this needs to be the last case.
       [(application? exp)
        (append (loop (operator exp))
                (apply append (map loop (operands exp))))]
              
       [else
        (error 'find-unbound-names "Unknown expression type ~e" exp)]))))


;; find-mutated-names: any -> (listof symbol)
;; Fixme: cache this.
;; Produces a set of the free names mutated in the expression.
(define (find-mutated-names exp)
  (unique/eq?
   (let loop ([exp exp])
     (cond
       [(self-evaluating? exp)
        '()]
       
       [(quoted? exp)
        '()]
       
       [(variable? exp)
        '()]
       
       [(definition? exp)
        (loop (definition-value exp))]
       
       [(if? exp)
        (append (loop (if-predicate exp))
                (loop (if-consequent exp))
                (loop (if-alternative exp)))]
       
       [(cond? exp)
        (loop (desugar-cond exp))]
       
       [(lambda? exp)
        (list-difference (loop (lambda-body exp))
                         (lambda-parameters exp))]
       
       [(begin? exp)
        (apply append (map loop (begin-actions exp)))]
       
       [(named-let? exp)
        (loop (desugar-named-let exp))]
       
       [(let*? exp)
        (loop (desugar-let* exp))]
       
       [(let? exp)
        (append (apply append (map loop (let-rhss exp)))
                (list-difference (apply append (map loop (let-body exp)))
                                 (let-variables exp)))]

       [(letrec? exp)
        (list-difference (append (apply append (map loop (let-rhss exp)))
                                 (apply append (map loop (let-body exp))))
                         (let-variables exp))]
       
       [(set!? exp)
        (cons (set!-name exp)
              (loop (set!-value exp)))]
       
       ;; Remember, this needs to be the last case.
       [(application? exp)
        (append (loop (operator exp))
                (apply append (map loop (operands exp))))]
       
       [else
        (error 'mutated? "Unknown expression type ~e" exp)]))))
  




;; expression selectors

(define (self-evaluating? exp)
  (cond
    [(number? exp) #t]
    [(string? exp) #t]
    [(boolean? exp) #t]
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


;; lambda-parameters: lambda-expression -> (listof identifier)
(define (lambda-parameters exp) 
  (let loop ([params (cadr exp)])
    (cond
      [(null? params)
       empty]
      [(pair? params)
       (cons (car params)
             (loop (cdr params)))]
      [else
       (list params)])))

;; Produces true if the lambda's last parameter is a rest parameter.
(define (lambda-has-rest-parameter? exp)
  (let loop ([params (cadr exp)])
    (cond
      [(null? params)
       #f]
      [(pair? params)
       (loop (cdr params))]
      [else
       #t])))

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
      `',(void)))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (cond? exp)
  (tagged-list? exp 'cond))

(define (desugar-cond exp)
  (let loop ([clauses (cdr exp)])
    (cond
      [(null? clauses)
       '(void)]
      [(null? (cdr clauses))
       (let* ([clause (car clauses)]
              [question (car clause)]
              [answer `(begin ,@(cdr clause))])
         (cond
           [(eq? question 'else)
            answer]
           [else
            `(if ,question 
                 ,answer
                 ',(void))]))]
      [else
       (let* ([clause (car clauses)]
              [question (car clause)]
              [answer `(begin ,@(cdr clause))])
         `(if ,question 
              ,answer
              ,(loop (cdr clauses))))])))


;;
;; Fixme: see if the parameter is mutated.  If so, box it.
;;
(define (parse-let exp cenv)
  (let ([vars (let-variables exp)]
        [rhss (let-rhss exp)]
        [body (let-body exp)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body) cenv #f)]
      [(= 1 (length vars))
       (let* ([mutated? (and (member (first vars) (find-mutated-names `(begin ,@body))) #t)]
              [let-body (parse `(begin ,@body)
                               (extend-lexical-environment/names 
                                cenv
                                (list (first vars))
                                (list mutated?))
                               #f)])
         (make-Let1 (parameterize ([current-defined-name (first vars)])
                      (parse (car rhss) (extend-lexical-environment/placeholders cenv 1) #f))
                    (if mutated?
                        (make-BoxEnv 0 let-body)
                        let-body)))]
      [else
       (let* ([rhs-cenv (extend-lexical-environment/placeholders cenv (length vars))]
              [mutated (find-mutated-names `(begin ,@body))]
              [any-mutated? (ormap (lambda (n) (and (member n mutated) #t)) vars)])
         (make-LetVoid (length vars)
                       (seq (append 
                             (map (lambda (var rhs index) 
                                    (make-InstallValue index 
                                                       (parameterize ([current-defined-name var])
                                                         (parse rhs rhs-cenv #f))
                                                       any-mutated?))
                                  vars
                                  rhss
                                  (build-list (length rhss) (lambda (i) i)))
                             (list (parse `(begin ,@body)
                                          (extend-lexical-environment/names 
                                           cenv vars
                                           (build-list (length vars) 
                                                       (lambda (i) 
                                                         any-mutated?)))
                                          #f))))
                       any-mutated?))])))


;; Letrec's currently doing a set! kind of thing.
(define (parse-letrec exp cenv)
  (let* ([vars (let-variables exp)]
         [rhss (let-rhss exp)]
         [body (let-body exp)]
         [n (length vars)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body) cenv #f)]
      [(and (andmap lambda? rhss)
            (empty? (list-intersection 
                     vars
                     (append (find-mutated-names body)
                             (apply append (map find-mutated-names rhss))))))
       (let ([new-cenv  (extend-lexical-environment/names cenv 
                                                          (reverse vars)
                                                          (build-list n (lambda (i) #f)))])
         ;; Semantics: allocate a closure shell for each lambda form in procs.
         ;; Install them in reverse order, so that the closure shell for the last element
         ;; in procs is at stack position 0.
         (make-LetRec (map (lambda (rhs name) (parameterize ([current-defined-name name])
                                                (parse rhs new-cenv #f)))
                           rhss
                           vars)
                      (parse `(begin ,@body) new-cenv #f)))]
      [else
       (let ([new-cenv  (extend-lexical-environment/boxed-names cenv (reverse vars))])
         (make-LetVoid (length vars)
                       (seq (append 
                             (map (lambda (var rhs index) 
                                    (make-InstallValue (- n 1 index)
                                                       (parameterize ([current-defined-name var])
                                                         (parse rhs new-cenv #f))
                                                       #t))
                                  vars
                                  rhss
                                  (build-list (length rhss) (lambda (i) i)))
                             (list (parse `(begin ,@body) new-cenv #f))))
                       #t))])))


(define (desugar-let* exp)
  (let ([body (let-body exp)])
    (let loop ([vars (let-variables exp)]
               [rhss (let-rhss exp)])
      (cond
        [(null? vars)
         `(begin ,@body)]
        [else
         `(let ([,(car vars) ,(car rhss)])
            ,(loop (cdr vars) (cdr rhss)))]))))




(define (desugar-named-let exp)
  `(letrec [(,(named-let-name exp)
             (lambda ,(named-let-variables exp)
               ,@(named-let-body exp)))]
     (,(named-let-name exp) ,@(named-let-rhss exp))))



(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (named-let-name exp)
  (cadr exp))
(define (named-let-variables exp)
  (map (lambda (clause)
         (car clause))
       (caddr exp)))
(define (named-let-rhss exp)
  (map (lambda (clause)
         (cadr clause))
       (caddr exp)))
(define (named-let-body exp)
  (cdddr exp))



;; any -> boolean
(define (let? exp)
  (tagged-list? exp 'let))

(define (letrec? exp)
  (tagged-list? exp 'letrec))

;; any -> boolean
(define (let*? exp)
  (tagged-list? exp 'let*))

;; let -> (listof symbol)
(define (let-variables exp)
  (map (lambda (clause)
         (car clause))
       (cadr exp)))

;; let -> (listof expr)
(define (let-rhss exp)
  (map (lambda (clause)
         (cadr clause))
       (cadr exp)))

;; let -> (listof expr)
(define (let-body exp)
  (cddr exp))


(define (set!? exp)
  (tagged-list? exp 'set!))

(define (set!-name exp)
  (cadr exp))

(define (set!-value exp)
  (caddr exp))