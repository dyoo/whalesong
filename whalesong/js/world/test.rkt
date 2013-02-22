#lang whalesong
(require whalesong/js/world
         whalesong/js
         whalesong/web-world)

;; Test of getting world events from arbitrary JavaScript function application.


;; js-function->procedure lifts JavaScript functions to regular
;; procedures that we can call.
(define setup-timer
  (js-function->procedure (js-eval "function(x) { window.sendTheTick = x; }")))

(define shutdown-timer
  (js-function->procedure (js-eval "function(_) { window.sendTheTick = void(0); }")))



;; We first define a new event handler type, by using make-js-world-event:
(define on-event (make-world-event-handler setup-timer shutdown-timer))



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
