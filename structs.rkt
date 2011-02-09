#lang racket/base

(provide (all-defined-out))

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





;; instruction sequences
(define-struct instruction-sequence (needs modifies statements) #:transparent)

(define empty-instruction-sequence (make-instruction-sequence '() '() '()))

(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))


(define (registers-needed s)
  (if (symbol? s) '() (instruction-sequence-needs s)))
(define (registers-modified s)
  (if (symbol? s) '() (instruction-sequence-modifies s)))
(define (statements s)
  (if (symbol? s) (list s) (instruction-sequence-statements s)))




;; Statements
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







;; Basic blocks
(define-struct basic-block (name stmts) #:transparent)
