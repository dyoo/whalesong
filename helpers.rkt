#lang typed/racket/base

(provide list-union list-difference unique)


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


;; Trying to work around what looks like a bug in typed racket:
(define string-sort (inst sort String String))

(: unique ((Listof Symbol) -> (Listof Symbol)))
(define (unique los)
  (let: ([ht : (HashTable Symbol Boolean) (make-hasheq)])
    (for ([l los])
      (hash-set! ht l #t))
    (map string->symbol
         (string-sort
          (hash-map ht (lambda: ([k : Symbol] [v : Boolean]) 
                                (symbol->string k)))
          string<?))))