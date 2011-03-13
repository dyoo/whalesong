#lang racket/base
(require "expression-structs.rkt")
(provide parse)

(define (parse exp)
  (cond
    [(self-evaluating? exp)
     (make-Constant exp)]
    
    [(quoted? exp)
     (make-Constant (text-of-quotation exp))]
    
    [(variable? exp)
     (make-Var exp)] 
    
    [(definition? exp)
     (make-Def (definition-variable exp)
               (parse (definition-value exp)))]
    
    [(if? exp)
     (make-Branch (parse (if-predicate exp))
                  (parse (if-consequent exp))
                  (parse (if-alternative exp)))]
    
    [(cond? exp)
     (parse (desugar-cond exp))]
    
    [(lambda? exp)
     (make-Lam (lambda-parameters exp)
               (make-Seq (map parse (lambda-body exp))))]
 
    [(begin? exp)
     (let ([actions (map parse (begin-actions exp))])
       (cond
         [(= 1 (length actions))
          (car actions)]
         [else
          (make-Seq actions)]))]
    
    [(let? exp)
     (parse-let exp)]
    
    [(let*? exp)
     (parse-let* exp)]
    
    [(letrec? exp)
     (parse-letrec exp)]
    
    [(application? exp)
     (make-App (parse (operator exp))
               (map parse (operands exp)))]
    
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
         


(define (parse-let exp)
  (let ([vars (let-variables exp)]
        [rhss (let-rhss exp)]
        [body (let-body exp)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body))]
      [(= 1 (length vars))
       (make-Let1 (car vars)
                  (parse (car rhss))
                  (parse `(begin ,@body)))]
      [else
       (make-Let vars
                 (map parse rhss)
                 (parse `(begin ,@body)))])))

(define (parse-letrec exp)
  (let ([vars (let-variables exp)]
        [rhss (let-rhss exp)]
        [body (let-body exp)])
    (cond 
      [(= 0 (length vars))
       (parse `(begin ,@body))]
      [else
       (make-LetRec vars
                    (map parse rhss)
                    (parse `(begin ,@body)))])))

(define (parse-let* exp)
  (parse
   (let ([body (let-body exp)])
     (let loop ([vars (let-variables exp)]
                [rhss (let-rhss exp)])
       (cond
         [(null? vars)
          `(begin ,@body)]
         [else
         `(let ([,(car vars) ,(car rhss)])
            ,(loop (cdr vars) (cdr rhss)))])))))
              
    
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