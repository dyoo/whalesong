#lang whalesong

;; This is a second semantics for the language that tries to go for speed,
;; at the expense of making things a little more complicated.
;;
;; It uses features in: http://docs.racket-lang.org/reference/unsafe.html
;; to reduce the number of runtime checks.
;;
;; We also manage the state as two separate values.
;;
;; Tape out-of-bounds errors at runtime should be properly reported with
;; source location.

(require (for-syntax racket/base)) 


(provide (all-defined-out))



(define-syntax MAX-DATA-SIZE
  (lambda (stx) #'30000))



;; Creates a new state, with a byte array of 30000 zeros, and
;; the pointer at index 0.
(define-syntax-rule (new-state) 
  (values (make-vector MAX-DATA-SIZE 0)
          0))


;; increment the data pointer
(define-syntax-rule (increment-ptr data ptr loc)
  (begin
    (set! ptr (+ ptr 1))))


;; decrement the data pointer
(define-syntax-rule (decrement-ptr data ptr loc)
  (set! ptr (- ptr 1)))


;; increment the byte at the data pointer
(define-syntax-rule (increment-byte data ptr)
  (vector-set! data ptr
                     (modulo
                      (+ (vector-ref data ptr)
                                  1)
                      256)))

;; decrement the byte at the data pointer
(define-syntax-rule (decrement-byte data ptr)
  (vector-set! data ptr
                     (modulo
                      (- (vector-ref data ptr)
                                  1)
                      256)))

;; print the byte at the data pointer
(define-syntax-rule (write-byte-to-stdout data ptr)
  (write-byte (vector-ref data ptr)))


;; ;; read a byte from stdin into the data pointer
;; (define-syntax-rule (read-byte-from-stdin data ptr)
;;   (vector-set! data ptr (let ([v (read-byte (current-input-port))])
;;                                  (if (eof-object? v)
;;                                      0
;;                                      v))))

;; Loops
(define-syntax-rule (loop data ptr body ...)
  (unless (= (vector-ref data ptr)
                      0)
    (let loop ()
      body ...
      (unless (= (vector-ref data ptr)
                          0)
        (loop)))))

