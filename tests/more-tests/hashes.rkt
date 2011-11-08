#lang planet dyoo/whalesong/base

(hash? 1)
(hash? "potatoes")
(hash? (make-hash))
(hash? (make-hash '((1 . one)
                    (2 . two)
                    (3 . three)
                    (4 . four))))
(hash? (make-hasheqv))
(hash? (make-hasheqv '((1 . one)
                       (2 . two)
                       (3 . three)
                       (4 . four))))
(hash? (make-hasheq))
(hash? (make-hasheq '((1 . one)
                      (2 . two)
                      (3 . three)
                      (4 . four))))

(make-hash)
(make-hasheqv)
(make-hasheq)

(make-hash '((1 . one)
             (2 . two)
             (3 . three)
             (4 . four)))
(make-hasheqv '((1 . one)
                (2 . two)
                (3 . three)
                (4 . four)))
(make-hasheq '((1 . one)
               (2 . two)
               (3 . three)
               (4 . four)))

(hash-ref (make-hash '((1 . one)
                       (2 . two)
                       (3 . three)))
          1)

(hash-ref (make-hash '((1 . one)
                       (2 . two)
                       (3 . three)))
          4
          (lambda () 'not-found))



(define words '("this" "is" "a" "test" "that" "is" "only" "a" "test!"))
(define ht (make-hash))
(for-each (lambda (w)
            (hash-set! ht
                       w
                       (add1 (hash-ref ht w (lambda () 0)))))
          words)
(hash-ref ht "this")
(hash-ref ht "is")
(hash-ref ht "a")
(hash-ref ht "test")
(hash-ref ht "that")
(hash-ref ht "only")
(hash-ref ht "test!")



(make-immutable-hash '((1 . one)
                       (2 . two)
                       (3 . three)
                       (4 . four)))
(make-immutable-hasheqv '((1 . one)
                          (2 . two)
                          (3 . three)
                          (4 . four)))
(make-immutable-hasheq '((1 . one)
                         (2 . two)
                         (3 . three)
                         (4 . four)))
(hash? (make-immutable-hash))
(hash? (make-immutable-hasheq))
(hash? (make-immutable-hasheqv))
(hash-equal? (make-immutable-hash))
(hash-equal? (make-immutable-hasheq))
(hash-equal? (make-immutable-hasheqv))
(hash-eq? (make-immutable-hash))
(hash-eq? (make-immutable-hasheq))
(hash-eq? (make-immutable-hasheqv))
(hash-eqv? (make-immutable-hash))
(hash-eqv? (make-immutable-hasheq))
(hash-eqv? (make-immutable-hasheqv))
(let* ([ht (make-immutable-hash)]
       [ht (hash-set ht 'name "danny")]
       [ht (hash-set ht 'email "dyoo@hashcollision.org")])
  (displayln (hash-ref ht 'name "unknown"))
  (displayln (hash-ref ht 'email "unknown"))
  (displayln (hash-ref ht 'phone "unknown")))



(let ([ht (make-hash '((1 . one)
                       (2 . two)))])
  (hash-remove! ht 1)
  (displayln (hash-ref ht 1 'not-there))
  (displayln (hash-ref ht 2 'not-there)))

(let* ([ht (make-immutable-hash '((1 . one)
                                 (2 . two)))])
  (hash-remove ht 1)
  (displayln (hash-ref ht 1 'not-there))
  (displayln (hash-ref ht 2 'not-there)))

(let* ([ht (make-immutable-hash '((1 . one)
                                 (2 . two)))]
       [ht (hash-remove ht 1)])
  (displayln (hash-ref ht 1 'not-there))
  (displayln (hash-ref ht 2 'not-there)))

(newline)
"hash-has-key"
(hash-has-key? (make-hash) 1)
(hash-has-key? (make-hash '((1 . one))) 1)
(hash-has-key? (make-immutable-hash) 1)
(hash-has-key? (make-immutable-hash '((1 . one))) 1)

(newline)
(hash 1 'one 2 'two)
(hasheqv 1 'one 2 'two)
(hasheq 1 'one 2 'two)


(newline)
(let* ([h1 (make-hash '((a . "A")))]
       [h2 (hash-copy h1)])
  (hash-set! h2 'a "alphabet")
  (displayln (hash-ref h1 'a))
  (displayln (hash-ref h2 'a)))


(newline)
(hash-count (make-hash '((a . a))))
(hash-count (make-immutable-hash '((b . b)
                                   (c . d))))

(newline)
(hash-equal? (make-hash))
(hash-equal? (make-hasheq))
(hash-equal? (make-hasheqv))
(hash-eq? (make-hash))
(hash-eq? (make-hasheq))
(hash-eq? (make-hasheqv))
(hash-eqv? (make-hash))
(hash-eqv? (make-hasheq))
(hash-eqv? (make-hasheqv))
