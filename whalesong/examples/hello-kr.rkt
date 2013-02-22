#lang whalesong/korean


(정의 (안녕 이름)
    (string-append "안녕" " " 이름))


(displayln (안녕 "danny"))


(정의 (f x)
    (조건부 
     [(= x 0)
      1]
     [다른
      (* x (f (- x 1)))]))

(f 50)


(정의-구조 사람 (이름 나이))
(make-사람 "danny" 32)
