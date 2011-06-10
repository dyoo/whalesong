#lang s-exp "../../lang/wescheme.ss"

(printf "case-lambda.rkt\n")

(define f
  (case-lambda
   [(x) (list x)]
   [(x y) (list y x)]
   [(x y z) (list z y x)]))

(check-expect (f 3) (list 3))
(check-expect (f 3 4) (list 4 3))
(check-expect (f 3 4 5) (list 5 4 3))