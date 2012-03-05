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


;; Let's attach the send-event function to a toplevel function on the window.
(void ((js-function->procedure (js-eval "function(x) { window.sendTheTick = x; }"))
       send-event))
;; js-function->procedure lifts JavaScript functions to regular
;; procedures that we can call.


(define (tick w v)
  (add1 w))


;; Finally, let's use our big bang:
(big-bang 0
          (on-event tick)    ;; Note the on-event here
          (stop-when (lambda (w v) (> w 5))))


;; Run this program.  A big-bang should be in progress and show 0.
;;
;; Next, open up your developer window, and call window.sendTheTick().
;; You should see the world respond.
