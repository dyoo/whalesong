#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)
(define-resource style.css)

(big-bang "don't care"
          (initial-view index.html))
