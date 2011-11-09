#lang planet dyoo/whalesong/base
#|
Why on earth are these here?
Because first, etc. don't work on cyclic lists:
(define web-colors
 (shared ([W (cons "white" G)]
          [G (cons "grey" W)])
   W))
(first web-colors)
fails with expected argument of type <list>.
But car/cdr still do the trick per email from mflatt, 10/20/2011.

So we suppress the built-in functions from lang/htdp-advanced
and provide these instead.
|#

(require (for-syntax racket/base))

(provide first second third fourth fifth sixth seventh eighth ninth
         rest)


(define (rest x)
  (cond
   [(pair? x)
    (cdr x)]
   [else
    (raise-type-error 'rest
                      "list with at least one element"
                      x)]))

(define-syntax (define-list-selectors stx)
  (syntax-case stx ()
    [(_ [(name ordinal) ...])
     (with-syntax ([(offset ...)
                    (build-list (length (syntax->list #'(name ...)))
                                (lambda (i) i))])
       #'(begin
           (define (name p)
             (pair-ref p offset 'name 'ordinal p))
           ...))]))

(define (pair-ref x offset name ordinal original)
  (cond
   [(pair? x)
    (cond
     [(= offset 0)
      (car x)]
     [else
      (pair-ref (cdr x) (sub1 offset) name ordinal original)])]
   [else
    (raise-type-error name
                      (format "list with ~a elements" ordinal)
                      original)]))
                                           
(define-list-selectors [[first one]
                        [second two]
                        [third three]
                        [fourth four]
                        [fifth five]
                        [sixth six]
                        [seventh seven]
                        [eighth eight]
                        [ninth nine]])
