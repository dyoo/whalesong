#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js/world))

(define-values (on-event send-event)
  (make-js-world-event))

((js-function (js-eval "function(x) { window.sendTheTick = x; }"))
 send-event)


(define (handle-event w v)
  (add1 w))

(big-bang 0
          (on-event handle-event)
          (stop-when (lambda (w) (> w 5))))
