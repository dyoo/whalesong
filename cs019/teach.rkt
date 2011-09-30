#lang racket/base

(require (for-template "../lang/base.rkt")
          stepper/private/shared
          racket/list)


(provide advanced-lambda/proc)



;; Raise a syntax error:
(define (teach-syntax-error form stx detail msg . args)
  (let ([form (or form (first (flatten (syntax->datum stx))))]
        [msg (apply format msg args)])
    (if detail
        (raise-syntax-error form msg stx detail)
        (raise-syntax-error form msg stx))))



(define (ensure-expression stx k)
  (if (memq (syntax-local-context) '(expression))
      (k)
      (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))



(define (something-else/kw stx)
  (if (identifier? stx)
      "a keyword"
      (something-else stx)))


;; Use for messages "expected ..., found <something else>"
(define (something-else v)
  (let ([v (syntax-e v)])
    (cond
     [(number? v) "a number"]
     [(string? v) "a string"]
     [(list? v) "a part"]
     [(struct? v) "an image"]
     [else "something else"])))



    ;; Use to generate nicer error messages than direct pattern
    ;; matching. The `where' argument is an English description
    ;; of the portion of the larger expression where a single
    ;; sub-expression was expected.
    (define (check-single-expression who where stx exprs will-bind)
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
	 "expected only one expression ~a, but found ~a extra part~a"
	 where
         (sub1 (length exprs))
         (if (> (length exprs) 2) "s" ""))))

(define (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))

;; The syntax error when a form's name doesn't follow a "("
(define (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "expected an open parenthesis before ~a, but found none" name))


(define (advanced-lambda/proc stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_  (name ...) . exprs)
        (let ([names (syntax->list (syntax (name ...)))])
          (for-each (lambda (name)
                      (unless (identifier? name)
                        (teach-syntax-error
                         'lambda
                         stx
                         name
                         "expected a variable, but found ~a"
                         (something-else/kw name))))
                    names)
          (let ([dup (check-duplicate-identifier names)])
            (when dup
              (teach-syntax-error
               'lambda
               stx
               dup
               "found a variable that is used more than once: ~a"
               (syntax-e dup))))
          (check-single-expression 'lambda 
                                   "for the function body"
                                   stx
                                   (syntax->list (syntax exprs))
                                   names)
          (syntax/loc stx (lambda (name ...) . exprs)))]
       [(_ arg-non-seq . exprs)
        (teach-syntax-error
         'lambda
         stx
         (syntax arg-non-seq)
         "expected at least one variable (in parentheses) after lambda, but found ~a"
         (something-else (syntax arg-non-seq)))]
       [(_)
        (teach-syntax-error
         'lambda
         stx
         #f
         "expected at least one variable (in parentheses) after lambda, but nothing's there")]
       [_else
        (bad-use-error 'lambda stx)]))))
