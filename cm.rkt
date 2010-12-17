#lang racket


;; Chapter 5.5 of the compiler


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
    ;; skipping cond
    [(application? exp)
     (compile-application exp target linkage)]
    [else
     (error 'compile "Unknown expression type ~e" exp)]))
    

(define (compile-self-evaluating exp target linkage) (error))
(define (compile-quoted exp target linkage) (error))
(define (compile-variable exp target linkage) (error))
(define (compile-assignment exp target linkage) (error))
(define (compile-definition exp target linkage) (error))
(define (compile-if exp target linkage) (error))
(define (compile-lambda exp target linkage) (error))
(define (compile-sequence seq target linkage) (error))
(define (compile-application exp target linkage) (error))



(define (self-evaluating? exp)
  (cond
    [(number? exp) #t]
    [(string? exp) #t]
    [else #f]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

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
(define (definiiton-variable exp)
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

(define (sequence->exp seq)
  (cond
    [(null? seq) '()]
    [(last-exp? seq)
     (first-exp seq)]
    [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))