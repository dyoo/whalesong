#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js))

(define js-plus
  (js-function (js-eval "function(x, y) { return x + y; }")))

(define js-minus
  (js-function (js-eval "function(x, y) { return x - y; }")))

"plus: " (js-plus 3 4)
"minus:" (js-minus 239748 23)