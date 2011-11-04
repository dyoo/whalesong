#lang planet dyoo/whalesong/base
(require (planet dyoo/whalesong/lang/private/shared))

;; boxes
(equal-hash-code (box 42))

;; bytes
(equal-hash-code #"testing")
;; chars

(equal-hash-code #\A)
(equal-hash-code #\B)
;; hashes

(equal-hash-code (make-hash '((1 . x)
                              (2 . y)
                              (3 . z))))
(define ht (make-hash))
(hash-set! ht 'self ht)
(hash-set! ht 'foo 4)
(equal-hash-code ht)

;; keywords
;

;; lists
(equal-hash-code (list 1 2 3 4 5))
(equal-hash-code (shared ([a (cons 1 b)]
                          [b (cons 2 a)])
                         a))

  
;; paths
;
;; placeholders
;;

;; strings
(equal-hash-code "Hello world")

;; structs
(define-struct thing (name age) #:mutable)
(equal-hash-code (make-thing "danny" 32))
(equal-hash-code (shared ([a (make-thing a a)]) a))

;; symbols
(equal-hash-code 'hello)

;; vectors
(equal-hash-code #(1 2 3 4 5))
(equal-hash-code (shared ([v (vector 1 2 v 3 v)]) v))
