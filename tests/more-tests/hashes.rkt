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