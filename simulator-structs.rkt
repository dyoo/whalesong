#lang typed/racket/base

(provide (all-defined-out))

(require "il-structs.rkt")


(define-struct: machine ([val : Any]
                         [proc : Any]
                         [env : (Listof Any)]
                         [control : (Listof frame)]

                         [pc : Natural]                  ;; program counter
                         [text : (Vectorof Statement)]   ;; text of the program
                         )
  #:transparent)


(define-struct: frame ([return : Symbol]
                       ;; TODO: add continuation marks
                       )
  #:transparent)

(define-struct: toplevel ([vals : (Listof Any)])
  #:transparent)


;; Primitive procedure wrapper
(define-struct: primitive-proc ([f : (Any * -> Any)])
  #:transparent)

;; Compiled procedure closures
(define-struct: closure ([label : Symbol]
                         [vals : (Listof Any)])
  #:transparent)
