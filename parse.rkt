#lang racket/base

(require "expression-structs.rkt"
         "lexical-env.rkt"
         "lexical-structs.rkt"
         racket/list)

(provide (rename-out (-parse parse)))

(define (-parse exp)
  (let* ([prefix (make-Prefix '())]
         [cenv (list prefix)])
    (let ([expr (parse exp cenv)])
      (make-Top prefix expr))))


;; find-prefix: CompileTimeEnvironment -> Natural
(define (find-prefix cenv)
  (cond
    [(empty? cenv)
     (error 'impossible)]
    [(Prefix? (first cenv))
     0]
    [else
     (add1 (find-prefix (rest cenv)))]))


;; find-variable*: Any CompileTimeEnvironment -> LexicalAddress
(define (find-variable* exp cenv)
  (let ([address (find-variable exp cenv)])
    (cond
      [(eq? address #f)
       (let* ([prefix-depth (find-prefix cenv)]
              [prefix (list-ref cenv prefix-depth)])
         (set-Prefix-names! prefix (append (Prefix-names prefix)
                                           (list exp)))
         (find-variable* exp cenv))]
      [else
       address])))



;; parse: Any CompileTimeEnvironment -> ExpressionCore
;; Compile an expression.
(define (parse exp cenv)
  (cond
    [(self-evaluating? exp)
     (make-Constant exp)]
    
    [(quoted? exp)
     (make-Constant (text-of-quotation exp))]
    
    [(variable? exp)
     (let ([address (find-variable* exp cenv)])
       (cond
         [(EnvLexicalReference? address)
          (make-LocalRef (EnvLexicalReference-depth address))]
         [(EnvPrefixReference? address)
          (make-ToplevelRef (EnvPrefixReference-depth address)
                            (EnvPrefixReference-pos address))]))]
    
    [(definition? exp)
     (let ([address (find-variable* exp cenv)])
       (cond
         [(EnvLexicalReference? address)
          (error 'parse "Can't define except in toplevel context")]
         [(EnvPrefixReference? address)
          (make-SetToplevel (EnvPrefixReference-depth address)
                            (EnvPrefixReference-pos address)
                            (EnvPrefixReference-name address)
                            (parse (definition-value exp) cenv))]))] 
    
    [(if? exp)
     (make-Branch (parse (if-predicate exp) cenv)
                  (parse (if-consequent exp) cenv)
                  (parse (if-alternative exp) cenv))]
    
    [(cond? exp)
     (parse (desugar-cond exp) cenv)]
    
    [(lambda? exp)
     ;; Fixme: need to know what variables are treated as free here!
     (let* ([prefix (list-ref cenv (find-prefix cenv))]
            [prefix-length (length (Prefix-names prefix))]
            [body-cenv (extend-lexical-environment/names 
                        '() 
                        (lambda-parameters exp))])
       (let ([lam-body (make-Seq (map (lambda (b)
                                        (parse b (cons prefix body-cenv)))
                                      (lambda-body exp)))])
         (cond [(= prefix-length (length (Prefix-names prefix)))
                (make-Lam (length (lambda-parameters exp))
                          lam-body)]
               [else
                (make-Lam (length (lambda-parameters exp))
                          (make-Seq (map (lambda (b)
                                           (parse b body-cenv))
                                         (lambda-body exp))))])))]
 
    [(begin? exp)
     (let ([actions (map (lambda (e)
                           (parse e cenv))
                         (begin-actions exp))])
       (cond
         [(= 1 (length actions))
          (car actions)]
         [else
          (make-Seq actions)]))]

    [(named-let? exp)
     (parse-named-let exp cenv)]
    
    [(let? exp)
     (parse-let exp cenv)]
    
    [(let*? exp)
     (parse-let* exp cenv)]
    
    [(letrec? exp)
     (parse-letrec exp cenv)]
    
    [(application? exp)
     (let ([cenv-with-scratch-space
            (extend-lexical-environment/placeholders cenv (length (operands exp)))])
       (make-App (parse (operator exp) cenv-with-scratch-space)
                 (map (lambda (rand) (parse rand cenv-with-scratch-space))
                      (operands exp))))]
    
    [else
     (error 'compile "Unknown expression type ~e" exp)]))



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
                 (void))]))]
      [else
       (let* ([clause (car clauses)]
              [question (car clause)]
              [answer `(begin ,@(cdr clause))])
         `(if ,question 
              ,answer
              ,(loop (cdr clauses))))])))
         


(define (parse-let exp cenv)
  (let ([vars (let-variables exp)]
        [rhss (let-rhss exp)]
        [body (let-body exp)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body))]
      [(= 1 (length vars))
       (make-Let1 (parse (car rhss) (extend-lexical-environment/placeholders cenv 1))
                  (parse `(begin ,@body)
                         (extend-lexical-environment/names cenv (list (first vars)))))]
      [else
       (let ([rhs-cenv (extend-lexical-environment/placeholders cenv (length vars))])
         (make-Let (length vars)
                   (map (lambda (rhs) (parse rhs rhs-cenv)) rhss)
                   (parse `(begin ,@body)
                          (extend-lexical-environment/names vars))))])))

(define (parse-letrec exp cenv)
  (let ([vars (let-variables exp)]
        [rhss (let-rhss exp)]
        [body (let-body exp)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body) cenv)]
      [else
       (let ([new-cenv (extend-lexical-environment/names cenv vars)])
         (make-LetRec (length vars)
                      (map (lambda (rhs) (parse rhs new-cenv)) rhss)
                      (parse `(begin ,@body) new-cenv)))])))


(define (parse-let* exp cenv)
  (parse
   (let ([body (let-body exp)])
     (let loop ([vars (let-variables exp)]
                [rhss (let-rhss exp)])
       (cond
         [(null? vars)
          `(begin ,@body)]
         [else
         `(let ([,(car vars) ,(car rhss)])
            ,(loop (cdr vars) (cdr rhss)))])))
   cenv))
              


(define (parse-named-let exp cenv)
  (parse
   `(letrec [(,(named-let-name exp)
	      (lambda ,(named-let-variables exp)
		,@(named-let-body exp)))]
      (,(named-let-name exp) ,@(named-let-rhss exp)))
   cenv))
  
    
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