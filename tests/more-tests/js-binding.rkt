#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js))

(define js-plus
  (js-function->procedure "function(x, y) { return x + y; }"))

(define js-minus
  (js-function->procedure "function(x, y) { return x - y; }"))

(define sleep
  (js-async-function->procedure"function(success, fail, n) { setTimeout(success, n) }"))


"plus: " (js-plus 3 4)
"wait for one second: " (sleep 1000)
"minus:" (js-minus 239748 23)


(for-each (lambda (x)
            (display x)
            (sleep 1000))
          '(hello world testing))


;; I need exception handling...
;;
;(define i-should-fail
;  (js-async-function->procedure  "function(success, fail) { fail('I should fail'); }"))
;(i-should-fail)
