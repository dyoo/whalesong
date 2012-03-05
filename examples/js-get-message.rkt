#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/js/world)
         (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/web-world))

;; Test of getting world events from arbitrary JavaScript function application.

;; We first define a new event handler type, by using make-js-world-event:
(define-values (on-message send)
  (make-js-world-event))

;; It gives us two values back:
;; 1.  An event handler that can be passed to big-bang
;; 2.  A raw JavaScript function that can fire events


;; Let's attach the send-event function to a toplevel function on the window.
(void ((js-function (js-eval "function(send) { $(window).bind('message', function(e) { send(e.originalEvent.data); })}"))
       send))
;; js-function lifts JavaScript functions to regular function we can call.


(define (get-message w v msg)
  (cons msg w))


;; Finally, let's use our big bang:
(big-bang '("Initial")
          (on-message get-message))    ;; Note the on-event here

