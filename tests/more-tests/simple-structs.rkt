#lang planet dyoo/whalesong
(define-struct p (f r))

(define p1 (make-p 3 4))
(p-f p1)
(p-r p1)
