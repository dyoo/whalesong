#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;

;;  Lexical environments


;; A toplevel prefix contains a list of toplevel variables.  Some of the
;; names may be masked out by #f.
(define-struct: Prefix ([names : (Listof (U Symbol False))])
  #:transparent)


(define-struct: NamedBinding ([name : Symbol]))


(define-type CompileTimeEnvironmentEntry (U Prefix ;; a prefix
                                            NamedBinding
                                            (Boxof NamedBinding) ;; A boxed local
                                            False))




;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.
(define-type CompileTimeEnvironment (Listof CompileTimeEnvironmentEntry))

;; A lexical address is a reference to an value in the environment stack.
(define-type LexicalAddress (U EnvLexicalReference EnvPrefixReference))


(define-struct: EnvLexicalReference ([depth : Natural]
                                     [unbox? : Boolean])
  #:transparent)
(define-struct: EnvPrefixReference ([depth : Natural]
                                    [pos : Natural]
                                    [name : Symbol])
  #:transparent)

(define-struct: EnvWholePrefixReference ([depth : Natural])
  #:transparent)
