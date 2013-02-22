#lang whalesong/base

(let myloop ([i 0] [acc 0])
  (cond
   [(< i 100)
    (myloop (add1 i) (+ acc i))]
   [else
    acc]))
