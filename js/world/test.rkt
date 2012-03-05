#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js/world)
         (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/web-world))

;; Test of getting world events from arbitrary JavaScript function application.

;; We first define a new event handler type, by using make-js-world-event:
(define-values (on-event send-event)
  (make-js-world-event))

;; It gives us two values back:
;; 1.  An event handler that can be passed to big-bang
;; 2.  A raw JavaScript function that can fire events


(void ((js-function (js-eval "function(x) { window.sendTheTick = x; }"))
       send-event))


(define (handle-event w v e f g)
  (displayln e)
  (displayln f)
  (displayln g)
  (add1 w))

(big-bang 0
          (on-event handle-event)
          (stop-when (lambda (w v) (> w 5))))
