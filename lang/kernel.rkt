#lang racket/base
(require (prefix-in math: (only-in racket/math pi sinh))
         (prefix-in math: (only-in mzlib/math e))
         (prefix-in racket: racket/base)
         (prefix-in advanced: lang/htdp-advanced))

(require (for-syntax racket/base)
         racket/local)

;; Special forms
(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx 
       (#%module-begin body ...))]))

;; datums
(define-syntax (-#%datum stx)
  (syntax-case stx ()
    [(_ . x)
     (syntax/loc stx
       (#%datum . x))]))


;; definitions
(define-syntax (-define stx)
  ;; FIXME: restrict define since we don't yet support keywords
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define x ...))]))

;; define-struct
(define-syntax (-define-struct stx)
  ;; FIXME: restrict define-struct since we don't yet support keywords
  (syntax-case stx ()
    [(_ x ...)
     (syntax/loc stx
       (define-struct x ... #:transparent))]))


;; constants
(define true #t)
(define false #f)
(define pi math:pi)
(define e math:e)
(define empty '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive function stubs

;; provide-stub-function
(define-syntax (provide-stub-function stx)
  (syntax-case stx ()
    [(_ name-or-name-pair ...)
     (with-syntax ([(provided-name ...)
                    (map (lambda (name-or-pair)
                           (syntax-case name-or-pair ()
                             [x
                              (identifier? #'x)
                              #'x]
                             [(x y)
                              #'x]))
                         (syntax->list #'(name-or-name-pair ...)))]
                   [(impl-name ...) 
                    (map (lambda (name)
                           (syntax-case name ()
                             [an-id
                              (identifier? #'an-id)
                              (datum->syntax name 
                                             (string->symbol
                                              (string-append "racket:"
                                                             (symbol->string 
                                                              (syntax-e name))))
                                             name)]
                             [(an-id an-impl-name)
                              #'an-impl-name]))
                         (syntax->list #'(name-or-name-pair ...)))])
       (syntax/loc stx
         (begin (begin (define (provided-name . args) 
                         (racket:apply impl-name args))
                       (provide provided-name))
                ...)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides
(provide (rename-out (-#%module-begin #%module-begin)
                     (-#%datum #%datum)
                     (#%app #%app)
                     (#%top-interaction #%top-interaction)
                     (#%top #%top)
                     (-define define)
                     (define-values define-values)
                     (let-values let-values)
                     (let*-values let*-values)
                     (-define-struct define-struct)
                     (if if)
                     (cond cond)
                     (else else)
                     (case case)
                     (quote quote)
                     (quasiquote quasiquote)
                     (unquote unquote)
                     (unquote-splicing unquote-splicing)
                     (lambda lambda)
                     (case-lambda case-lambda)
                     (let let)
                     (let* let*)
                     (letrec letrec)
                     (letrec-values letrec-values)
                     (local local)
                     (begin begin)
                     (begin0 begin0)
                     (set! set!)
                     (and and)
                     (or or)
                     (when when)
                     (unless unless)
                     (require require)
                     (for-syntax for-syntax)
                     (define-for-syntax define-for-syntax)
                     (begin-for-syntax begin-for-syntax)
                     (prefix-in prefix-in)
		     (only-in only-in)
                     (provide provide)
		     (planet planet)
		     (all-defined-out all-defined-out)
		     (all-from-out all-from-out)
		     (except-out except-out)
		     (rename-out rename-out)
		     (struct-out struct-out)
                     (define-syntax define-syntax)
                     (define-syntaxes define-syntaxes)
                     (let/cc let/cc)
		     (with-continuation-mark with-continuation-mark)
                     
                     (true true)
                     (false false)
                     (pi pi)
                     (e e)
                     (empty empty)
                     (eof eof)
                     (null null)))


(define (-identity x) x)

(define (-undefined? x)
  (letrec ([y y])
    (eq? x y)))


(provide-stub-function #;xml->s-exp
                       #;js-object?

                       write
                       display
                       newline
                       current-print
                       current-continuation-marks
		       continuation-mark-set?
                       continuation-mark-set->list
                       for-each
                       ;; make-thread-cell
                       make-struct-type
                       make-struct-field-accessor
                       make-struct-field-mutator
                       struct-type?
                       struct-constructor-procedure?
                       struct-predicate-procedure?
                       struct-accessor-procedure?
                       struct-mutator-procedure?
                       procedure-arity
		       procedure-arity-includes?
		       make-arity-at-least
		       arity-at-least?
		       arity-at-least-value
                       apply
                       values
                       call-with-values
                       compose
                       current-inexact-milliseconds
                       current-seconds
                       not
                       void
                       random
                       sleep
                       (identity -identity)
                       raise
                       error

                       make-exn
                       make-exn:fail
                       make-exn:fail:contract
                       make-exn:fail:contract:arity
                       make-exn:fail:contract:variable
                       make-exn:fail:contract:divide-by-zero

                       exn-message
                       exn-continuation-marks

		       exn?
		       exn:fail?
		       exn:fail:contract?
		       exn:fail:contract:arity?
		       exn:fail:contract:variable?
		       exn:fail:contract:divide-by-zero?


                       *
                       -
                       +
                       =
                       (=~ advanced:=~)
                       /
                       sub1
                       add1
                       <
                       >
                       <=
                       >=
                       abs
                       quotient
                       remainder
                       modulo
                       max
                       min
                       gcd
                       lcm
                       floor
                       ceiling
                       round
		       truncate
                       numerator
                       denominator
                       expt
                       exp
                       log
                       sin
                       cos
                       tan
                       asin
                       acos
                       atan
                       (sinh advanced:sinh)
                       (cosh advanced:cosh)
                       (sqr advanced:sqr)
                       sqrt
                       integer-sqrt
                       make-rectangular
                       make-polar
                       real-part
                       imag-part
                       angle
                       magnitude
                       (conjugate advanced:conjugate)
                       (sgn advanced:sgn)
                       inexact->exact
                       exact->inexact
                       number->string
                       string->number
                       procedure?
                       pair?
                       (cons? advanced:cons?)
                       (empty? advanced:empty?)
                       null?
                       (undefined? -undefined?)
		       immutable?
                       void?
                       symbol?
                       string?
                       char?
                       boolean?
                       vector?
                       struct?
                       eof-object?
                       bytes?
                       byte?
                       number?
                       complex?
                       real?
                       rational?
                       integer?
                       exact?
                       inexact?
                       odd?
                       even?
                       zero?
                       positive?
                       negative?
                       box?
                       hash?
                       eq?
                       eqv?
                       equal?
                       (equal~? advanced:equal~?)
                       (false? advanced:false?)
                       (boolean=? advanced:boolean=?)
                       (symbol=? advanced:symbol=?)
                       cons
                       car
                       cdr
                       caar
                       cadr
                       cdar
                       cddr
                       caaar
                       caadr
                       cadar
                       cdaar
                       cdadr
                       cddar
                       caddr
                       cdddr
                       cadddr
                       (rest advanced:rest)
                       (first advanced:first)
                       (second advanced:second)
                       (third advanced:third)
                       (fourth advanced:fourth)
                       (fifth advanced:fifth)
                       (sixth advanced:sixth)
                       (seventh advanced:seventh)
                       (eighth advanced:eighth)
                       length
                       list?
                       list
                       list*
                       list-ref
                       list-tail
                       append
                       reverse
                       map
                       andmap
                       ormap
                       memq
                       memv
                       member
                       memf
                       assq
                       assv
                       assoc
                       remove
                       filter
                       foldl
                       foldr
                       (quicksort advanced:quicksort)
                       sort
                       (argmax advanced:argmax)
                       (argmin advanced:argmin)
                       build-list
                       box
                       box-immutable
                       unbox
                       set-box!
                       make-hash
                       make-hasheq
                       hash-set!
                       hash-ref
                       hash-remove!
                       hash-map
                       hash-for-each
                       make-string
                       (replicate advanced:replicate)
                       string
                       string-length
                       string-ref
                       string=?
                       string-ci=?
                       string<?
                       string>?
                       string<=?
                       string>=?
                       string-ci<?
                       string-ci>?
                       string-ci<=?
                       string-ci>=?
                       substring
                       string-append
                       string->list
                       list->string
                       string-copy
                       string->symbol
                       symbol->string
                       format
                       printf
                       (string->int advanced:string->int)
                       (int->string advanced:int->string)
                       (explode advanced:explode)
                       (implode advanced:implode)
                       (string-alphabetic? advanced:string-alphabetic?)
                       (string-ith advanced:string-ith)
                       (string-lower-case? advanced:string-lower-case?)
                       (string-numeric? advanced:string-numeric?)
                       (string-upper-case? advanced:string-upper-case?)
                       (string-whitespace? advanced:string-whitespace?)
                       build-string
                       string->immutable-string
                       string-set!
                       string-fill!
                       make-bytes
                       bytes
                       bytes->immutable-bytes
                       bytes-length
                       bytes-ref
                       bytes-set!
                       subbytes
                       bytes-copy
                       bytes-fill!
                       bytes-append
                       bytes->list
                       list->bytes
                       bytes=?
                       bytes<?
                       bytes>?
                       make-vector
                       vector
                       vector-length
                       vector-ref
                       vector-set!
                       vector->list
                       list->vector
                       build-vector
                       char=?
                       char<?
                       char>?
                       char<=?
                       char>=?
                       char-ci=?
                       char-ci<?
                       char-ci>?
                       char-ci<=?
                       char-ci>=?
                       char-alphabetic?
                       char-numeric?
                       char-whitespace?
                       char-upper-case?
                       char-lower-case?
                       char->integer
                       integer->char
                       char-upcase
                       char-downcase

                       
                       call-with-current-continuation
                       call/cc
		       call-with-continuation-prompt
		       abort-current-continuation
		       default-continuation-prompt-tag
                       make-continuation-prompt-tag
		       continuation-prompt-tag?

		       make-reader-graph
		       make-placeholder
		       placeholder-set!)
