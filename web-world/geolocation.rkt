#lang s-exp "../lang/base.rkt"
(require "../js/world.rkt"
         "../js.rkt")

;; Create a new event handler type
(define-values (on-geo-change locationCallback)
  (make-js-event-type))

(define start-up-geo
  (js-function->procedure
   "function(locationCallback) {
        navigator.geolocation.watchPosition(
            function(evt) { locationCallback(evt.latitude, evt.longitude); })}"))

;; TODO: adjust the FFI so we can start-up and shutdown this more easily.
(start-up-geo locationCallback)

