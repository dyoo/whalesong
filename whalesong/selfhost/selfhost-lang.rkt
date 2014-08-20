#lang whalesong
;;;
;;;
;;;

;;; Note: define-predicate is not defined as a no-op here.
;;;       We need the error message to find where to add our own predicates.

(provide define-struct:
         define-type
         inst
         make-parameter
         parameterize
         bytes?
         path?
         sort
         natural?
         vector-copy
         bytes->string/utf-8 ; is values
         path->string
         ; no-ops
         :
         ann
         log-debug
         ensure-type-subsetof)

(require "selfhost-parameters.rkt")
(require (for-syntax racket/base))

(define (bytes? o) #f)  ; TODO
(define (path? o) #f)   ; TODO

(define (sort xs <<)
  (define (merge xs ys)
    (cond 
      [(empty? xs) ys]
      [(empty? ys) xs]
      [else (define x (first xs))
            (define y (first ys))
            (if (<< x y)
                (cons x (merge (rest xs) ys))
                (cons y (merge xs (rest ys))))]))
  (define (split xs)
    (define n (length xs))
    (cond [(<= n 1) (list xs '())]
          [else     (let loop ([ys '()] [xs xs] [i 0])
                      (if (> (* 2 i) (- n 1))
                          (list ys xs)
                          (loop (cons (first xs) ys) (rest xs) (+ i 1))))]))
  (define n (length xs))
  (cond [(< n 2) xs]
        [else (define halves (split xs))                  
              (merge (sort (first halves) <<)
                     (sort (second halves) <<))]))




; define-struct: uses the same syntax as the one from typed/racket, but it 
; simply discards the types and expand into standard Whalesong define-struct.

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(ds: struct-name ([field-name : type] ...) options ...)
     #'(define-struct struct-name (field-name ...) options ...)]))


(define-syntax (define-type stx) #'(void))
(define-syntax (: stx) #'(void))
(define-syntax (ensure-type-subsetof stx) #'(void))
(define-syntax (ann stx) (syntax-case stx () [(_ expr type) #'expr]))

(define-syntax (inst stx)
  (syntax-case stx ()
    [(_ e ignore ...)
     #'e]))

(define (log-debug . _) (void))

(define (natural? o) (and (number? o) (integer? o) (not (negative? o))))

(require whalesong/lang/for)

(define (vector-copy vec [start 0] [end (vector-length vec)])
  (define n (- end start))
  (define v (make-vector n #\space))
  (for ([i (in-range start end)]
        [j (in-range 0 n)])
    (vector-set! v j (vector-ref vec i)))
  v)

(define bytes->string/utf-8 values)

(define (path->string x)
  (error 'todo-implement-me))