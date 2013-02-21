#lang typed/racket/base
(require (for-syntax racket/base))

;; Working around what appears to be a bug in Typed Racket
;; by implementing my own promises.

(provide my-delay my-force MyPromise)


(define-struct: Sentinel ())


(define-struct: (a) MyPromise ([forced? : Boolean]
                               [thunk : (-> a)]
                               [val : (U Sentinel a)])
  #:mutable)


(define-syntax (my-delay stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       (make-MyPromise #f
		       (lambda () expr ...)
		       (make-Sentinel)))]))

(: my-force (All (a) (MyPromise a) -> a))
(define (my-force a-promise)
  (cond
   [(MyPromise-forced? a-promise)
    (define val (MyPromise-val a-promise))
    (if (Sentinel? val)
	(error 'force "Impossible")
	val)]
   [else
    (define val ((MyPromise-thunk a-promise)))
    (set-MyPromise-val! a-promise val)
    (set-MyPromise-forced?! a-promise #t)
    val]))