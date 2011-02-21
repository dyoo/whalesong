#lang typed/racket/base

(provide list-union list-difference)


(: list-union ((Listof Symbol) (Listof Symbol) -> (Listof Symbol)))
(define (list-union s1 s2)
  (cond [(null? s1) s2]
        [(memq (car s1) s2)
         (list-union (cdr s1) s2)]
        [else (cons (car s1) (list-union (cdr s1) s2))]))


(: list-difference ((Listof Symbol) (Listof Symbol) -> (Listof Symbol)))
(define (list-difference s1 s2)
  (cond [(null? s1) '()]
        [(memq (car s1) s2)
         (list-difference (cdr s1) s2)]
        [else
         (cons (car s1) (list-difference (cdr s1) s2))]))