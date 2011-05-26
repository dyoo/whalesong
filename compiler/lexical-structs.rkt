#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;

;;  Lexical environments


;; A toplevel prefix contains a list of toplevel variables.  Some of the
;; names may be masked out by #f.
(define-struct: Prefix ([names : (Listof (U False Symbol GlobalBucket ModuleVariable))])
  #:transparent)

(define-struct: GlobalBucket ([name : Symbol])
  #:transparent)


;; A ModuleLocator is an identifier for a Module.
(define-struct: ModuleLocator ([name : Symbol]
                            [real-path : (U Symbol Path)])
  #:transparent)


(define-struct: ModuleVariable ([name : Symbol]
                                [module-name : ModuleLocator])
  #:transparent)


(define-struct: NamedBinding ([name : Symbol]
                              [parameter? : Boolean]
                              [boxed? : Boolean])
  #:transparent)


(define-type ParseTimeEnvironmentEntry (U Prefix ;; a prefix
                                            NamedBinding
                                            False))




;; A compile-time environment is a (listof (listof symbol)).
;; A lexical address is either a 2-tuple (depth pos), or 'not-found.
(define-type ParseTimeEnvironment (Listof ParseTimeEnvironmentEntry))

;; A lexical address is a reference to an value in the environment stack.
(define-type LexicalAddress (U EnvLexicalReference EnvPrefixReference))


(define-struct: EnvLexicalReference ([depth : Natural]
                                     [unbox? : Boolean])
  #:transparent)

(define-struct: EnvPrefixReference ([depth : Natural]
                                    [pos : Natural])
  #:transparent)

(define-struct: EnvWholePrefixReference ([depth : Natural])
  #:transparent)


;; An environment reference is either lexical or referring to a whole prefix.
(define-type EnvReference (U EnvLexicalReference
                             EnvWholePrefixReference))