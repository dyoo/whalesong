#lang typed/racket/base
(provide (all-defined-out))

(define-type Expression (U Constant Quote Var Assign Branch Def Lam Seq App))
(define-struct: Constant ([v : Any]) #:transparent)
(define-struct: Quote ([text : Any]) #:transparent)
(define-struct: Var ([id : Symbol]) #:transparent)
(define-struct: Assign ([variable : Symbol]
                        [value : Expression]) #:transparent)
(define-struct: Branch ([predicate : Expression]
                        [consequent : Expression]
                        [alternative : Expression]) #:transparent)
(define-struct: Def ([variable : Symbol] 
                     [value : Expression]) #:transparent)
(define-struct: Lam ([parameters : (Listof Symbol)]
                     [body : (Listof Expression)]) #:transparent)
(define-struct: Seq ([actions : (Listof Expression)]) #:transparent)
(define-struct: App ([op : Expression]
                     [rands : (Listof Expression)]) #:transparent)

(: last-exp? ((Listof Expression) -> Boolean))
(define (last-exp? seq) 
  (null? (cdr seq)))

(: first-exp ((Listof Expression) -> Expression))
(define (first-exp seq) (car seq))

(: rest-exps ((Listof Expression) -> (Listof Expression)))
(define (rest-exps seq) (cdr seq))



;; instruction sequences
(define-struct: instruction-sequence ([needs : (Listof Symbol)]
                                      [modifies : (Listof Symbol)]
                                      [statements : (Listof Any)]) #:transparent)
(define empty-instruction-sequence (make-instruction-sequence '() '() '()))

(: make-label (Symbol -> Symbol))
(define make-label
  (let ([n 0])
    (lambda (l)
      (set! n (add1 n))
      (string->symbol (format "~a~a" l n)))))