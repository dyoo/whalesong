#lang typed/racket/base/no-check
(require (for-syntax racket/base
                     syntax/parse))
                     
;; Provides helpers for use with Typed Racket programs.

(provide ensure-type-subsetof)


;; Usage: (ensure-type-subsetof subtype supertype)
;;
;; Statically errors out if subtype is not within supertype.
;;
(define-syntax (ensure-type-subsetof stx)
  (syntax-parse stx
    [(_ subtype:id supertype:id)
     ;; begin-splicing
     (with-syntax ([x (syntax/loc stx x)])
       #`(void (lambda () (ann (values (ann #,(syntax/loc stx (error 'fail))
                                            subtype)) supertype))))]))




#|
(define-type T0 (U 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p
                   'q 'r 's 't 'u 'v 'w 'x 'y 'z))
(define-type T1 (U 'a
                   'e
                   'i
                   'o
                   'u))
(ensure-type-subsetof T1 T0)



(define-struct: Id ([name : Symbol]))
(define-struct: Num ([datum : Number]))
(define-struct: Add ([lhs : Expr]
                     [rhs : Expr]));
(define-type Expr 
  (U Id 
     ;; Num     ;; Uncomment to correct the type error
     Add))
(define-type ConstantExpr (U Id Num))

;; And if we mess up at least it errors out at compile time
(ensure-type-subsetof ConstantExpr Expr)
|#