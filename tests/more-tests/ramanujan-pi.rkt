#lang planet dyoo/whalesong
;; Srinivasa Ramanujan's infinite series for approximating pi.

(define (sum f a b)
  (cond
    [(= a b)
     (f a)]
    [else
     (+ (f a)
        (sum f (add1 a) b))]))

(define (fact x)
  (cond
    [(= x 0)
     1]
    [else
     (* x (fact (sub1 x)))]))


(define (1/pi-approx n)
  (* (/ (* 2 (sqrt 2))
        9801)
     (sum (lambda (k)
            (/ (* (fact (* 4 k))
                  (+ 1103 (* 26390 k)))
               (* (expt (fact k) 4)
                  (expt 396 (* 4 k)))))
          0
          n)))

(define (pi-approx n)
  (/ 1 (1/pi-approx n)))


(pi-approx 10)