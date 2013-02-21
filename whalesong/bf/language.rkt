#lang planet dyoo/whalesong

(require "semantics.rkt"
         (for-syntax racket/base))

(provide greater-than
         less-than
         plus
         minus
         period
         comma
         brackets
         (rename-out [my-module-begin #%module-begin]))




;; Every module in this language will make sure that it
;; uses a fresh state.  We create one, and then within
;; the lexical context of a my-module-begin, all the
;; other forms will refer to current-state.
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([current-data (datum->syntax stx 'current-data)]
                   [current-ptr (datum->syntax stx 'current-ptr)])
       (syntax/loc stx
         (#%plain-module-begin
          (define-values (current-data current-ptr) (new-state))
          (define (run)
            (begin body ... (void)))
          (run))))]))


;; In order to produce good runtime error messages
;; for greater-than and less-than, we latch onto 
;; the syntax object for dear life, since it has
;; information about where it came from in the
;; source syntax.
;;
;; The #'#,stx nonsense below allows us to pass the
;; syntax object.  The semantics can then raise an
;; appropriate syntactic error with raise-syntax-error
;; if it sees anything bad happen at runtime.
(define-syntax (greater-than stx)
  (with-syntax ([current-data (datum->syntax stx 'current-data)]
                [current-ptr (datum->syntax stx 'current-ptr)])
    (syntax-case stx ()
      [(_)
       (quasisyntax/loc stx
         (increment-ptr current-data current-ptr
                        (srcloc '#,(syntax-source stx)
                                '#,(syntax-line stx)
                                '#,(syntax-column stx)
                                '#,(syntax-position stx)
                                '#,(syntax-span stx))))])))


(define-syntax (less-than stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([current-data (datum->syntax stx 'current-data)]
                   [current-ptr (datum->syntax stx 'current-ptr)])
       (quasisyntax/loc stx
         (decrement-ptr current-data current-ptr
                        (srcloc '#,(syntax-source stx)
                                '#,(syntax-line stx)
                                '#,(syntax-column stx)
                                '#,(syntax-position stx)
                                '#,(syntax-span stx)))))]))


(define-syntax (plus stx)
  (with-syntax ([current-data (datum->syntax stx 'current-data)]
                [current-ptr (datum->syntax stx 'current-ptr)])
    (syntax/loc stx
      (increment-byte current-data current-ptr))))

(define-syntax (minus stx)
  (with-syntax ([current-data (datum->syntax stx 'current-data)]
                [current-ptr (datum->syntax stx 'current-ptr)])
    (syntax/loc stx
      (decrement-byte current-data current-ptr))))

(define-syntax (period stx)
  (with-syntax ([current-data (datum->syntax stx 'current-data)]
                [current-ptr (datum->syntax stx 'current-ptr)])
    (syntax/loc stx 
      (write-byte-to-stdout current-data current-ptr))))

(define-syntax (comma stx)
  (with-syntax ([current-data (datum->syntax stx 'current-data)]
                [current-ptr (datum->syntax stx 'current-ptr)])
    (syntax/loc stx
      (read-byte-from-stdin current-data current-ptr))))

(define-syntax (brackets stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([current-data (datum->syntax stx 'current-data)]
                   [current-ptr (datum->syntax stx 'current-ptr)])
       (syntax/loc stx
         (loop current-data current-ptr body ...)))]))