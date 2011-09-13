#lang s-exp "../base.rkt"

(require [for-syntax syntax/parse]
         [for-syntax syntax/struct]
         [for-syntax racket])



(provide define: lambda: define-struct: and: or: not:)

(define-for-syntax (parse-sig stx)
  (syntax-case stx (->)
    [(A ... -> R)
     (with-syntax ([(A ...) (map parse-sig (syntax->list #'(A ...)))]
                   [R (parse-sig #'R)])
       #'(proc: (A ... -> R)))]
    [_ stx]))

(define-for-syntax (parse-sigs stxs)
  (map parse-sig (syntax->list stxs)))

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ sn ([f : S] ...))
     (with-syntax ([(names ...) 
                    (build-struct-names #'sn
                                        (syntax->list #'(f ...)) 
                                        #f #f)]
                   [term stx]
                   [(S ...) (parse-sigs #'(S ...))])
       (with-syntax ([sig-name (datum->syntax #'sn
                                              (string->symbol
                                               (string-append
                                                (symbol->string
                                                 (syntax->datum #'sn))
                                                "$")))]
                     [cnstr (syntax-case #'(names ...) ()
                              [(struct:name-id constructor misc ...)
                               #'constructor])]
                     [(_sid _ctr _id? setters ...)
                      (build-struct-names #'sn
                                          (syntax->list #'(f ...))
                                          #t #f)]
                     [pred (syntax-case #'(names ...) ()
                             [(struct:name-id const predicate misc ...)
                              #'predicate])])
         #'(begin
             (define-values (names ...)
               (let ()
                 (begin
                   (define-struct sn (f ...) #:transparent #:mutable)
                   (let ([cnstr 
                          (lambda (f ...)
                            (let ([wrapped-args
                                   (let loop ([sigs (list S ... )]
                                              [args (list f ...)]
                                              [sig-srcs (syntax->list #'(S ...))]
                                              [n 1])
                                     (if (empty? sigs)
                                         empty
                                         (cons (wrap (first sigs) 
                                                     (first args)
                                                     )
                                               (loop (rest sigs) 
                                                     (rest args)
                                                     (rest sig-srcs)
                                                     (add1 n)))))])
                              (apply cnstr wrapped-args)))]
                         [setters
                          (lambda (struct-inst new-val)
                            (setters struct-inst (wrap S new-val)))]
                         ...)
                     (values names ...)))))
             ;; This could be a define below, but it's a define-values
             ;; due to a bug in ISL's local.  See users@racket-lang.org
             ;; thread, 2011-09-03, "splicing into local".  Should not
             ;; be necessary with next release.
             (define-values (sig-name) 
               (first-order-sig pred)))))]))

(define (not-sig-error src)
  (error 'signature-violation "not a valid signature: ~e" src))

(define (wrap sig val)
  (if (signature? sig)
      ((signature-wrapper sig) val)
      (not-sig-error)))

(provide Number$ String$ Char$ Boolean$ Any$ Sig: Listof: Vectorof:)

(define-struct signature (pred wrapper ho?))

(define-syntax (Listof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(let ([s S]
               [sig-src #'S]
               [term-src #'term])
           (if (signature? s)
               (if (signature-ho? s)
                   (make-signature list?
                                   (lambda (v)
                                     (map (lambda (e) (wrap s e)) v))
                                   true
                                   term-src)
                   (let ([pred (lambda (v)
                                 (and (list? v)
                                      (andmap (signature-pred s) v)))])
                     (make-signature pred
                                     (lambda (v)
                                       (if (pred v)
                                           v
                                           (if (list? v)
                                               (raise-syntax-error
                                                'signature-violation
                                                "not an appropriate list"
                                                v
                                                #f
                                                (list sig-src))
                                               (raise-syntax-error
                                                'signature-violation
                                                "not a list"
                                                v
                                                #f
                                                (list term-src)))))
                                     false
                                     term-src)))
               (not-sig-error sig-src))))]))

(define-syntax (Vectorof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(let ([s S]
               [sig-src #'S]
               [term-src #'term])
           (if (signature? s)
               (if (signature-ho? s)
                   (make-signature vector?
                                   (lambda (v)
                                     (list->vector
                                      (map (lambda (e) (wrap s e))
                                           (vector->list v))))
                                   true
                                   term-src)
                   (let ([pred (lambda (v)
                                 (and (vector? v)
                                      (andmap (signature-pred s)
                                              (vector->list v))))])
                     (make-signature pred
                                     (lambda (v)
                                       (if (pred v)
                                           v
                                           (if (vector? v)
                                               (raise-syntax-error
                                                'signature-violation
                                                "not an appropriate vector"
                                                v
                                                #f
                                                (list sig-src))
                                               (raise-syntax-error
                                                'signature-violation
                                                "not a vector"
                                                v
                                                #f
                                                (list term-src)))))
                                     false
                                     term-src)))
               (not-sig-error sig-src))))]))

(define (first-order-sig pred?)
  (make-signature pred?
                  (lambda (v)
                    (if (pred? v)
                        v
                        (error
                         'signature-violation
                         (format "value ~a failed the signature" v)
                         #;#f
                         #;#f
                         #;(list term-src))))
                  #f))

(define-syntax (Sig: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([Sp (parse-sig #'S)]
                   [term stx])
       (if (eq? #'Sp #'S) ;; currently means S is NOT (... -> ...)
           #'(first-order-sig S)
           #'Sp))]))

(define-syntax (Number$ stx)
  (syntax-case stx (Number$)
    [Number$
     (with-syntax ([term stx])
       #'(first-order-sig number?))]))

(define-syntax (String$ stx)
  (syntax-case stx (String$)
    [String$
     (with-syntax ([term stx])
       #'(first-order-sig string?))]))

(define-syntax (Char$ stx)
  (syntax-case stx (char$)
    [Char$
     (with-syntax ([term stx])
       #'(first-order-sig char?))]))

(define-syntax (Boolean$ stx)
  (syntax-case stx (Boolean$)
    [Boolean$
     (with-syntax ([term stx])
       #'(first-order-sig boolean?))]))
                                              
(define-syntax (Any$ stx)
  (syntax-case stx (Any$)
    [Any$
     (with-syntax ([term stx])
       #'(first-order-sig (lambda (_) true)))]))

;; proc: is for internal use only.
;; Stand-alone procedural signatures are defined using Sig:; e.g.,
;; (define n->n (Sig: (Number$ -> Number$)))
;; In all other cases, the macros invoke parse-sig, which takes care of
;; automatically wrapping (proc: ...) around procedure signatures.
(define-syntax (proc: stx)
  (syntax-case stx (->)
    [(_ (A ... -> R))
     (with-syntax ([(args ...) (generate-temporaries #'(A ...))]
                   [(A ...) (parse-sigs #'(A ...))]
                   [R (parse-sig #'R)]
                   [term stx])
       #'(make-signature
          procedure?
          (lambda (v)
            (if (procedure? v)
                (lambda (args ...)
                  (wrap R (v (wrap A args #'A) ...)))
                (raise-syntax-error
                 'signature-violation
                 "not a procedure"
                 v
                 #f
                 (list #'term))))
          true
          #'term))]))

(define-syntax (define: stx)
  (syntax-case stx (: ->)
    [(_ id : S exp)
     (identifier? #'id)
     (with-syntax ([S (parse-sig #'S)])
       #'(define id (wrap S exp)))]
    [(_ (f [a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       #'(define f (lambda: ([a : Sa] ...) -> Sr exp)))]))

(define-syntax (lambda: stx)
  (syntax-case stx (: ->)
    [(_ ([a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       #'(lambda (a ...)
                     (let ([a (wrap Sa a)] ...)
                       (wrap Sr exp))))]))     

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term stx])
       #'(first-order-sig
          (lambda (x)
            (let loop ([sigs (list S ...)]
                       [sig-srcs (syntax->list #'(S ...))])
              (if (empty? sigs)
                  false
                  (let ([s (first sigs)])
                    (if (signature? s)
                        (if (signature-ho? s)
                            (raise-syntax-error
                             'signature-violation
                             "or: cannot combine higher-order signature" 
                             #'term
                             #f
                             (list #;(signature-src s)))
                            (or ((signature-pred s) x)
                                (loop (rest sigs) (rest sig-srcs))))
                        (not-sig-error (first sig-srcs)))))))))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term stx])
       #'(first-order-sig
          (lambda (x)
            (let loop ([sigs (list S ...)]
                       [sig-srcs (syntax->list #'(S ...))])
              (if (empty? sigs)
                  true
                  (let ([s (first sigs)])
                    (if (signature? s)
                        (if (signature-ho? s)
                            (raise-syntax-error
                             'signature-violation
                             "and: cannot combine higher-order signature" 
                             #'term
                             #f
                             (list #;(signature-src s)))
                            (and ((signature-pred s) x)
                                 (loop (rest sigs) (rest sig-srcs))))
                        (not-sig-error (first sig-srcs)))))))))]))

(define-syntax (not: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(let ([s S]
               [sig-src #'S]
               [term-src #'term])
           (if (signature? s)
               (if (signature-ho? s)
                   (raise-syntax-error
                    'signature-violation
                    "not: cannot negate higher-order signature" 
                    #'term)
                   (first-order-sig (lambda (x) (not ((signature-pred s) x)))))
               (not-sig-error sig-src))))]))
