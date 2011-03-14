#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;

;;  Lexical environments


;; A toplevel prefix contains a list of toplevel variables.  Some of the
;; names may be masked out by #f.
(define-struct: Prefix ([names : (Listof (U Symbol False))])
  #:transparent)




(define-type CompileTimeEnvironmentEntry (U Prefix ;; a prefix
                                            Symbol
                                            (Boxof Symbol) ;; A boxed local
                                            False
                                            #;FunctionExtension
                                            #;LocalExtension
                                            #;TemporaryExtension))


;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.
(define-type CompileTimeEnvironment (Listof CompileTimeEnvironmentEntry))

;; A lexical address is a reference to an value in the environment stack.
(define-type LexicalAddress (U LocalAddress PrefixAddress))
(define-struct: LocalAddress ([depth : Natural]
                              [unbox? : Boolean])
  #:transparent)
(define-struct: PrefixAddress ([depth : Natural]
                               [pos : Natural]
                               [name : Symbol])
  #:transparent)