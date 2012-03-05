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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With this infrastructure, we can make a world program that responds to window postMessage.  For example,
;; we can present a log of all the messages we receive.

(define-struct world (time messages))

(define (read-message w v msg)
  (make-world (world-time w)
              (cons (format "at time ~a: ~s"
                            (world-time w)
                            msg)
                    (world-messages w))))

(define (tick w v)
  (make-world (add1 (world-time w))
              (world-messages w)))


;; Finally, let's use our big bang:
(big-bang (make-world 0 '())
          (on-tick tick 1)
          (on-message read-message))

