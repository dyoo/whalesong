#lang whalesong


;; shuffle: vector -> vector
;; Reorders the contents of a vector according to the Fisher-Yates shuffling algorithm.
(define (shuffle! a-vec)
  (letrec ([iter (lambda (i)
                   (cond
                    [(<= i 0)
                     a-vec]
                    [else
                     (let* ([index (random (add1 i))]
                            [t (vector-ref a-vec i)])                       
                       (vector-set! a-vec i (vector-ref a-vec index))
                       (vector-set! a-vec index t)
                       (iter (sub1 i)))]))])
    (iter (sub1 (vector-length a-vec)))))
 
(shuffle! (vector))
(shuffle! (vector 'one))
(shuffle! (vector 1 2))
(shuffle! (vector "red" "white" "blue"))
(shuffle! (list->vector (string->list "abcdefghijklmnopqrstuvwxyz")))
