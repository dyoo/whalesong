#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;

;;  Lexical environments

;; A toplevel prefix contains a list of toplevel variables.
(define-struct: Prefix ([names : (Listof Symbol)])
  #:transparent)

;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.
(define-type CompileTimeEnvironment (Listof (U (Listof Symbol)
                                               Prefix)))
(define-type LexicalAddress (U LocalAddress PrefixAddress))

(define-struct: LocalAddress ([depth : Natural]
                              [pos : Natural])
  ;; These need to be treated transparently for equality checking.
  #:transparent)
(define-struct: PrefixAddress ([depth : Natural]
                               [pos : Natural]
                               [name : Symbol])
  ;; These need to be treated transparently for equality checking.
  #:transparent)