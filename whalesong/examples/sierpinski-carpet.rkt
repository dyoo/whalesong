#lang whalesong/base
(require whalesong/image)
;; Sierpenski carpet.
;; http://rosettacode.org/wiki/Sierpinski_carpet#Scheme

(define SQUARE (square 5 "solid" "red"))
(define SPACE (square 5 "solid" "white"))

(define (carpet n)
  (local [(define (in-carpet? x y)
           (cond ((or (zero? x) (zero? y))
                  #t)
                 ((and (= 1 (remainder x 3)) (= 1 (remainder y 3)))
                  #f)
                 (else
                  (in-carpet? (quotient x 3) (quotient y 3)))))]
 
  (letrec ([outer (lambda (i)
                    (cond
                      [(< i (expt 3 n))                       
                       (local ([define a-row
                                 (letrec ([inner 
                                           (lambda (j)
                                             (cond [(< j (expt 3 n))
                                                    (cons (if (in-carpet? i j)
                                                              SQUARE
                                                              SPACE)
                                                          (inner (add1 j)))]
                                                   [else
                                                    empty]))])
                                   (inner 0))])
                         (cons (apply beside a-row)
                               (outer (add1 i))))]
                      [else
                       empty]))])
    (apply above (outer 0)))))


(carpet 4)
