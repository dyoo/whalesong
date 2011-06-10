#lang s-exp "../../lang/wescheme.rkt"

"math.rkt"

(check-expect (number? pi) true)
(check-expect (number? e) true)

(check-within pi 22/7 0.1)
(check-within e 2.718 0.1)

(check-expect (=~ 3 4 1) true)
(check-expect (=~ 3 4 .9) false)


(check-expect (< 3 4) true)
(check-expect (< 4 3) false)
(check-expect (< 3 3) false)

(check-expect (> 3 4) false)
(check-expect (> 4 3) true)
(check-expect (> 4 4) false)

(check-expect (<= 3 4) true)
(check-expect (<= 4 3) false)
(check-expect (<= 3 3) true)

(check-expect (>= 3 4) false)
(check-expect (>= 4 3) true)
(check-expect (>= 4 4) true)


(check-expect (abs 3) 3)
(check-expect (abs -3) 3)

(check-expect (quotient 42 2) 21)
(check-expect (remainder 42 2) 0)

(check-expect (modulo 5 3) 2)

(check-expect (max 3 4 5) 5)
(check-expect (max 5) 5)

(check-expect (min 3 4 5) 3)
(check-expect (min 5) 5)


(check-expect (gcd 3 4) 1)
(check-expect (gcd 5 10 20) 5)


(check-expect (lcm 3 4) 12)
(check-expect (lcm 5 10 20) 20)


(check-expect (floor 3) 3)
(check-expect (ceiling 3) 3)

(check-expect (round 3) 3)
(check-expect (round 3) 3)

(check-expect (floor 3.5) 3.0)
(check-expect (ceiling 3.5) 4.0)

(check-expect (floor -3.5) -4.0)
(check-expect (ceiling -3.5) -3.0)





"math.rkt end"