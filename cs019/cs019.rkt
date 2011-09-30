#lang s-exp "../lang/base.rkt"

;; Like the big whalesong language, but with additional ASL restrictions.

(current-print-mode "constructor")


(require (for-syntax racket/base syntax/stx racket/match))


(require "cs019-pre-base.rkt")
(provide (rename-out [cs019-lambda lambda]
                     [cs019-define define]
                     [cs019-when when]
                     [cs019-unless unless]
                     [cs019-case case]))


(require (prefix-in whalesong: "../lang/whalesong.rkt"))
(provide (except-out (filtered-out
                      (lambda (name)
                        (match name
                          [(regexp #rx"^whalesong:(.+)$" (list _ real-name))
                           real-name]
                          [else
                           #f]))
                      (all-from-out "../lang/whalesong.rkt"))
                     if
                     cond
                     case
                     member
                     define
                     lambda
                     unless
                     when)

         string-ith
         replicate
         int->string
         string->int
         explode
         implode
         string-numeric?
         string-alphabetic?
         string-whitespace?
         string-upper-case?
         string-lower-case?)
                     

(require "../image.rkt")
(provide (all-from-out "../image.rkt"))

(require "../web-world.rkt")
(provide (all-from-out "../web-world.rkt"))

(require "../resource.rkt")
(provide (all-from-out "../resource.rkt"))





(define-for-syntax (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))



    


;; Raise a syntax error:
(define-for-syntax (teach-syntax-error form stx detail msg . args)
  (let ([form (if (eq? form '|function call|)
                  form
                  #f)] ; extract name from stx
        [msg (apply format msg args)])
    (if detail
        (raise-syntax-error form msg stx detail)
        (raise-syntax-error form msg stx))))

(define-for-syntax (teach-syntax-error* form stx details msg . args)
  (let ([exn (with-handlers ([exn:fail:syntax?
                              (lambda (x) x)])
               (apply teach-syntax-error form stx #f msg args))])
    (raise
     (make-exn:fail:syntax
      (exn-message exn)
      (exn-continuation-marks exn)
      details))))



;; The syntax error when a form's name doesn't follow a "("
(define-for-syntax (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "found a use of `~a' that does not follow an open parenthesis"
   name))

(define-for-syntax (something-else v)
  (let ([v (syntax-e v)])
    (cond
      [(number? v) "a number"]
      [(string? v) "a string"]
      [else "something else"])))

;; verify-boolean is inserted to check for boolean results:
(define-for-syntax (verify-boolean b where)
  (with-syntax ([b b]
                [where where])
    (quasisyntax/loc #'b
      (let ([bv b])
        (if (or (eq? bv #t) (eq? bv #f))
            bv
            #,(syntax/loc #'b
                (whalesong:#%app raise
                                 (make-exn:fail:contract
                                  (format "~a: question result is not true or false: ~e" 'where bv)
                                  (current-continuation-marks)))))))))


(define-syntax (-cond stx)
  (syntax-case stx ()
    [(_)
     (teach-syntax-error
      'cond
      stx
      #f
      "expected a question--answer clause after `cond', but nothing's there")]
    [(_ clause ...)
     (let* ([clauses (syntax->list (syntax (clause ...)))]
            [check-preceding-exprs
             (lambda (stop-before)
               (let/ec k
                 (for-each (lambda (clause)
                             (if (eq? clause stop-before)
                                 (k #t)
                                 (syntax-case clause ()
                                   [(question answer)
                                    (begin
                                      (unless (and (identifier? (syntax question))
                                                   (free-identifier=? (syntax question)
                                                                      #'else))
                                        (local-expand-for-error (syntax question) 'expression null))
                                      (local-expand-for-error (syntax answer) 'expression null))])))
                           clauses)))])
       (let ([checked-clauses
              (map
               (lambda (clause)
                 (syntax-case clause (else)
                   [(else answer)
                    (let ([lpos (memq clause clauses)])
                      (when (not (null? (cdr lpos)))
                        (teach-syntax-error
                         'cond
                         stx
                         clause
                         "found an `else' clause that isn't the last clause ~
                                    in its `cond' expression"))
                      (with-syntax ([new-test (syntax #t) ])
                        (syntax/loc clause (new-test answer))))]
                   [(question answer)
                    (with-syntax ([verified 
                                   (verify-boolean #'question 'cond)])
                      (syntax/loc clause (verified answer)))]
                   [()
                    (check-preceding-exprs clause)
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a question--answer clause, but found an empty clause")]
                   [(question?)
                    (check-preceding-exprs clause)
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a clause with a question and answer, but found a clause with only one part")]
                   [(question? answer? ...)
                    (check-preceding-exprs clause)
                    (let ([parts (syntax->list clause)])
                      ;; to ensure the illusion of left-to-right checking, make sure 
                      ;; the question and first answer (if any) are ok:
                      (unless (and (identifier? (car parts))
                                   (free-identifier=? (car parts) #'else))
                        (local-expand-for-error (car parts) 'expression null))
                      (unless (null? (cdr parts))
                        (local-expand-for-error (cadr parts) 'expression null))
                      ;; question and answer (if any) are ok, raise a count-based exception:
                      (teach-syntax-error*
                       'cond
                       stx
                       parts
                       "expected a clause with one question and one answer, but found a clause with ~a parts"
                       (length parts)))]
                   [_else
                    (teach-syntax-error
                     'cond
                     stx
                     clause
                     "expected a question--answer clause, but found ~a"
                     (something-else clause))]))
               clauses)])
         ;; Add `else' clause for error (always):
         (let ([clauses (append checked-clauses 
                                (list 
                                 (with-syntax ([error-call (syntax/loc stx (whalesong:#%app raise (make-exn:fail:contract "cond: all question results were false" (current-continuation-marks))))])
                                   (syntax [else error-call]))))])
           (with-syntax ([clauses clauses])
             (syntax/loc stx (cond . clauses))))))]
    [_else (bad-use-error 'cond stx)]))

(provide (rename-out [-cond cond]))






(define-syntax (-if stx)
  (syntax-case stx ()
    [(_ test then else)
     (with-syntax ([new-test (verify-boolean #'test 'if)])
       (syntax/loc stx
         (if new-test
             then
             else)))]
    [(_ . rest)
     (let ([n (length (syntax->list (syntax rest)))])
       (teach-syntax-error
        'if
        stx
        #f
        "expected one question expression and two answer expressions, but found ~a expression~a"
        (if (zero? n) "no" n)
        (if (= n 1) "" "s")))]
    [_else (bad-use-error 'if stx)]))

(provide (rename-out [-if if]))




;; Use to generate nicer error messages than direct pattern
;; matching. The `where' argument is an English description
;; of the portion of the larger expression where a single
;; sub-expression was expected.
(define-for-syntax (check-single-expression who where stx exprs will-bind)
  (when (null? exprs)
    (teach-syntax-error
     who
     stx
     #f
     "expected an expression ~a, but nothing's there"
     where))
  (unless (null? (cdr exprs))
    ;; In case it's erroneous, to ensure left-to-right reading, let's
    ;;  try expanding the first expression. We have to use
    ;;  `will-bind' to avoid errors for unbound ids that will actually
    ;;  be bound. Since they're used as stopping points, we may miss
    ;;  some errors after all. It's worth a try, though. We also
    ;;  have to stop at advanced-set!, in case it's used with
    ;;  one of the identifiers in will-bind.
    (when will-bind
      (local-expand-for-error (car exprs) 'expression (cons #'advanced-set!
                                                            will-bind)))
    ;; First expression seems ok, report an error for 2nd and later:
    (teach-syntax-error
     who
     stx
     (cadr exprs)
     "expected only one expression ~a, but found ~a extra part"
     where
     (if (null? (cddr exprs))
         "one"
         "at least one"))))






(define 1-LET "1-letter string")
(define 1-LETTER (format "~a" 1-LET))
(define 1-LETTER* (format "list of ~as" 1-LET))
(define NAT "natural number")

;; Symbol Any -> Boolean 
;; is this a 1-letter string?
(define (1-letter? tag s)
  (unless (string? s) (err tag "expected a ~a, but received a string: ~e" 1-LETTER s))
  (= (string-length s) 1))

;; Symbol Any -> Boolean 
;; is s a list of 1-letter strings
;; effect: not a list, not a list of strings 
(define (1-letter*? tag s)
  (unless (list? s) (err tag "expected a ~a, but received: ~e" 1-LETTER* s))
  (for-each 
   (lambda (c) 
     (unless (string? c) (err tag "expected a ~a, but received: ~e" 1-LETTER* c)))
   s)
  (andmap (compose (lambda (x) (= x 1)) string-length) s))


(define (err tag msg-format . args)
  (raise 
   (make-exn:fail:contract
    (apply format (string-append (symbol->string tag) ": " msg-format) args)
    (current-continuation-marks))))

(define (a-or-an after)
  (if (member (string-ref (format "~a" after) 0) '(#\a #\e #\i #\o #\u))
      "an" "a"))

(define cerr 
  (case-lambda
    [(tag check-result format-msg actual)
     (unless check-result
       (err tag (string-append "expected " (a-or-an format-msg) " " format-msg ", but received ~e") actual))]
    [(tag check-result format-msg actual snd)
     (unless check-result
       (err tag (string-append "expected " (a-or-an format-msg) " " format-msg " for the ~a argument, but received ~e")
            snd actual))]))

(define string-ith
  (lambda (s n)
    (define f "exact integer in [0, length of the given string]")
    (cerr 'string-ith (string? s) "string" s "first")
    (cerr 'string-ith (and (number? n) (integer? n) (>= n 0)) NAT n "second")
    (let ([l (string-length s)]) 
      (cerr 'string-ith (< n l) f n "second"))
    (string (string-ref s n))))



(define replicate 
  (lambda (n s1)
    (cerr 'replicate (and (number? n) (exact-integer? n) (>= n 0)) NAT n)
    (cerr 'replicate (string? s1) "string" s1)
    (apply string-append (build-list n (lambda (i) s1)))))

(define int->string 
  (lambda (i) 
    (cerr 'int->string 
          (and (exact-integer? i) (or (<= 0 i 55295) (<= 57344 i 1114111)))
          "exact integer in [0,55295] or [57344 1114111]"
          i)
    (string (integer->char i))))

(define string->int 
  (lambda (s) 
    (cerr 'string->int (1-letter? 'string->int s) 1-LETTER s)
    (char->integer (string-ref s 0))))


(define explode 
  (lambda (s)
    (cerr 'explode (string? s) "string" s)
    (map string (string->list s))))

(define implode
  (lambda (los)
    (cerr 'implode (1-letter*? 'implode los) 1-LETTER* los)
    (apply string-append los)))



(define string-numeric? 
  ;; is this: (number? (string->number s)) enough?
  (lambda (s1)
    (cerr 'string-numeric? (string? s1) "string" s1)
    (andmap char-numeric? (string->list s1))))

(define string-alphabetic? 
  (lambda (s1)
    (cerr 'string-alphabetic? (string? s1) "string" s1)
    (andmap char-alphabetic? (string->list s1))))


(define string-whitespace? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s)  "string" s)
    (andmap char-whitespace? (string->list s))))

(define string-upper-case? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s) "string" s)
    (andmap char-upper-case? (string->list s))))


(define string-lower-case? 
  (lambda (s)
    (cerr 'string-lower-case? (string? s) "string" s)
    (andmap char-lower-case? (string->list s))))


;; ASL's member returns booleans.
(define (-member x L)
  (cond
    [(eq? (member x L) #f) #f]
    [else #t]))

(provide (rename-out [-member member]
                     [-member member?]))

