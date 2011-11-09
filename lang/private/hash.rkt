#lang s-exp "../kernel.rkt"

(provide hash-map hash-for-each)

(define (hash-map a-hash f)
  (unless (hash? a-hash)
    (raise-type-error 'hash-map "hash" a-hash))
  (unless (and (procedure? f) (procedure-arity-includes? f 2))
    (raise-type-error 'hash-map "procedure (arity 2)" f))
  (let loop ([keys (hash-keys a-hash)])
    (if (null? keys)
        '()
        (cons (f (car keys) (hash-ref a-hash (car keys)))
              (loop (rest keys))))))


(define (hash-for-each a-hash f)
  (unless (hash? a-hash)
    (raise-type-error 'hash-for-each "hash" a-hash))
  (unless (and (procedure? f) (procedure-arity-includes? f 2))
    (raise-type-error 'hash-for-each "procedure (arity 2)" f))
  (let loop ([keys (hash-keys a-hash)])
    (if (null? keys)
        (void)
        (begin
          (f (car keys) (hash-ref a-hash (car keys)))
          (loop (rest keys))))))


