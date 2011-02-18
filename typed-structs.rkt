#lang typed/racket/base
(provide (all-defined-out))

(define-type Expression (U Constant Quote Var Assign Branch Def Lam Seq App))
(define-struct: Constant ([v : Any]) #:transparent)
(define-struct: Quote ([v : Any]) #:transparent)
(define-struct: Var ([id : Symbol]) #:transparent)
(define-struct: Assign ([id : Symbol]
                        [expr : Expression]) #:transparent)
(define-struct: Branch ([test : Expression]
                        [consequent : Expression]
                        [alternative : Expression]) #:transparent)
(define-struct: Def ([id : Symbol] 
                     [expr : Expression]) #:transparent)
(define-struct: Lam ([ids : (Listof Symbol)]
                     [body : Expression]) #:transparent)
(define-struct: Seq ([es : (Listof Expression)]) #:transparent)
(define-struct: App ([op : Expression]
                     [rands : (Listof Expression)]) #:transparent)
