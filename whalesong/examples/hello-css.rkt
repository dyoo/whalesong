#lang whalesong/base
(require whalesong/web-world
         whalesong/resource)

(define-resource hello-css.css)
(define-resource hello-css-main.html)

(big-bang 0
          (initial-view hello-css-main.html)
          (to-draw (lambda (w v) v)))

"done"
