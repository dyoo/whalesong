#lang typed/racket/base
(require racket/list)
(provide list-union list-difference list-intersection unique/eq? unique/equal?)


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

(: list-intersection ((Listof Symbol) (Listof Symbol) -> (Listof Symbol)))
(define (list-intersection s1 s2)
  (cond [(null? s1) '()]
        [(memq (car s1) s2)
         (cons (car s1) (list-intersection (cdr s1) s2))]
        [else
         (list-intersection (cdr s1) s2)]))


;; Trying to work around what looks like a bug in typed racket:
(define string-sort (inst sort String String))

(: unique/eq? ((Listof Symbol) -> (Listof Symbol)))
(define (unique/eq? los)
  (let: ([ht : (HashTable Symbol Boolean) (make-hasheq)])
    (for ([l los])
      (hash-set! ht l #t))
    (map string->symbol
         (string-sort
          (hash-map ht (lambda: ([k : Symbol] [v : Boolean]) 
                                (symbol->string k)))
          string<?))))



(: unique/equal? (All (A) ((Listof A) -> (Listof A))))
(define (unique/equal? lst)
  (cond
    [(empty? lst)
     empty]
    [(member (first lst) (rest lst))
     (unique/equal? (rest lst))]
    [else
     (cons (first lst)
           (unique/equal? (rest lst)))]))