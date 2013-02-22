#lang whalesong
(require whalesong/web-world
         whalesong/resource)

(define-resource index.html)
(define-resource style.css)

(big-bang "don't care"
          (initial-view index.html))
