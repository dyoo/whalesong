#lang planet dyoo/whalesong

(map (lambda (x y) (+ x y)) (list 1 2 3) (list 4 5 6))
(map + (list 1 2 3) (list 4 5 6))
(map + (list 1 2 3) (list 4 5 6) (list 7 8 9))
(map + (list 1 2 3))
