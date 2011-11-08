#lang racket/base
(require (prefix-in racket: (only-in racket/math pi sinh cosh sqr
                                     sgn conjugate))
         (prefix-in racket: racket/base)
         racket/provide
	 racket/local
	 (for-syntax racket/base)
         racket/stxparam
         
         (only-in '#%paramz
                  exception-handler-key
                  parameterization-key
                  break-enabled-key))

(require (prefix-in kernel: '#%kernel))


(provide exception-handler-key
         parameterization-key
         break-enabled-key)


(provide define-syntax-parameter
         syntax-parameterize)

;; constants
(define pi racket:pi)
(define e (racket:exp 1))


(define my-current-print-mode "write")
(define current-print-mode
  (case-lambda
    [() my-current-print-mode]
    [(v) (set! my-current-print-mode v)]))

(provide current-print-mode)


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
(provide pi
         e
         null
         eof
         #%plain-module-begin
	 #%module-begin
	 #%datum
	 #%app
         #%plain-app
	 #%top-interaction
	 #%top
         module
         define
	 define-values
         let-syntax
	 let-values
	 let*-values
	 define-struct
         struct
         if
	 cond
	 else
	 case
	 quote
	 unquote
	 unquote-splicing
	 lambda
	 case-lambda
	 let
	 let*
	 letrec
	 letrec-values
	 local
	 begin
	 begin0
	 set!
	 and
	 or
	 when
	 unless
	 require
	 for-syntax
         for-template
	 define-for-syntax
	 begin-for-syntax
	 prefix-in
	 only-in
         rename-in
         except-in
	 provide
	 planet
	 all-defined-out
	 all-from-out
         prefix-out
	 except-out
	 rename-out
	 struct-out
         filtered-out
         combine-in
         protect-out
         combine-out

         
         define-syntax-rule
	 define-syntax
	 define-syntaxes


         let/cc
	 with-continuation-mark

         hash?
         hash-eq?
         hash-eqv?
         hash
         hasheqv
         hasheq
         make-hash
         make-hasheqv
         make-hasheq
         make-immutable-hash
         make-immutable-hasheqv
         make-immutable-hasheq
         hash-copy
         hash-ref
         hash-set!
         hash-set
         hash-remove!
         hash-remove
         equal-hash-code
         hash-count
         


         ;; Kernel inlinable
         *
	 -
	 +
	 =
	 /
	 sub1
	 add1
	 <
	 >
	 <=
	 >=
         cons
         car
         cdr
         list
         list?
         pair?
         null?
         not
         eq?
         values

         ;; The version of apply in racket/base is doing some stuff that
         ;; we are not handling yet.  So we expose the raw apply here instead.
         (rename-out [kernel:apply apply])
         call-with-values

         gensym


         srcloc
         make-srcloc
         srcloc?
         srcloc-source
         srcloc-line
         srcloc-column
         srcloc-position
         srcloc-span


         make-struct-type
         make-struct-field-accessor
         make-struct-field-mutator
         struct-type?

         exn:fail
         struct:exn:fail
         prop:exn:srclocs


         current-inexact-milliseconds
         current-seconds
         
         
         ;; needed for cs019-local
         #%stratified-body
         )


(define (-identity x) x)

(define (-undefined? x)
  (letrec ([y y])
    (eq? x y)))



;; Many of these should be pushed upward rather than stubbed, so that
;; Racket's compiler can optimize these.
(provide-stub-function

 current-output-port
 current-print 



 write
 write-byte
 display
 newline
 displayln

 
 current-continuation-marks

;;  continuation-mark-set?
;;  continuation-mark-set->list

;;  struct-constructor-procedure?
;;  struct-predicate-procedure?
;;  struct-accessor-procedure?
;;  struct-mutator-procedure?

;;  make-arity-at-least
;;  arity-at-least?
;;  arity-at-least-value


;;  compose
;;  current-inexact-milliseconds
;;  current-seconds
  void
  random
;;  sleep
;;  (identity -identity)

raise  
error
raise-type-error
raise-mismatch-error

make-exn
make-exn:fail
make-exn:fail:contract
make-exn:fail:contract:arity
make-exn:fail:contract:variable
make-exn:fail:contract:divide-by-zero

exn-message
exn-continuation-marks


;;  exn?
;;  exn:fail?
;;  exn:fail:contract?
;;  exn:fail:contract:arity?
;;  exn:fail:contract:variable?
;;  exn:fail:contract:divide-by-zero?
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
  sinh
  cos
  cosh
  tan
  asin
  acos
  atan
  sqr
  sqrt
  integer-sqrt
  sgn
  make-rectangular
  make-polar
  real-part
  imag-part
  angle
  magnitude
  conjugate
  inexact->exact
  exact->inexact
  number->string
  string->number
  procedure?
  procedure-arity
  procedure-arity-includes?
  procedure-rename
  ;;  (undefined? -undefined?)
;;  immutable?
void?
symbol?
string?
char?
boolean?
vector?
struct?
;;  bytes?
byte?
number?
complex?
real?
rational?
integer?
exact-integer?
exact?
exact-nonnegative-integer?
inexact?
odd?
even?
zero?
positive?
negative?
box?
;;  hash?

  equal?
  eqv?

  caar
  cadr
;;  cdar
;;  cddr
;;  caaar
;;  caadr
;;  cadar
;;  cdaar
;;  cdadr
;;  cddar
;;  caddr
;;  cdddr
;;  cadddr
  length
  list*
  list-ref
;;  list-tail
  append
  reverse
  for-each
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
;;  sort
  box
;;  box-immutable
  unbox
  set-box!
;;  make-hash
;;  make-hasheq
;;  hash-set!
;;  hash-ref
;;  hash-remove!
;;  hash-map
;;  hash-for-each
  make-string
  string
  string-length
  string-ref
  string=?
  string<?
  string>?
  string<=?
  string>=?
  string-ci=?
  string-ci<?
  string-ci>?
  string-ci<=?
  string-ci>=?

  string-copy
  substring
  string-append
  string->list
  list->string
;;  string-copy
string->symbol
symbol->string
  format
  printf
  fprintf
;;  string->immutable-string
  string-set!
;;  string-fill!
;;  make-bytes
;;  bytes
;;  bytes->immutable-bytes
;;  bytes-length
;;  bytes-ref
;;  bytes-set!
;;  subbytes
;;  bytes-copy
;;  bytes-fill!
;;  bytes-append
;;  bytes->list
;;  list->bytes
;;  bytes=?
;;  bytes<?
;;  bytes>?
  make-vector
  vector
  vector-length
  vector-ref
  vector-set!
  vector->list
  list->vector
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

  
  ;; these are defined in bootstrapped-primitives in Whalesong's compiler package
  call-with-current-continuation
  call/cc

  ;;  call-with-continuation-prompt
  ;;  abort-current-continuation
  default-continuation-prompt-tag
  make-continuation-prompt-tag
  continuation-prompt-tag?

  make-reader-graph
  make-placeholder
  placeholder-set!

  eof-object?
  read-byte


  hash-has-key?
  )





(provide set-car! set-cdr!)

(define (set-car! x v)
  (error 'set-car! "Not available outside JavaScript context."))

(define (set-cdr! x v)
  (error 'set-car! "Not available outside JavaScript context."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
