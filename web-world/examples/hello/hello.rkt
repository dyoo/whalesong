#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html "index.html")

(big-bang "don't care"
          (initial-view index.html))
