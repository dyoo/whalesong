#lang s-exp "../lang/base.rkt"
(require "../js/world.rkt"
         "../js.rkt")

(provide on-geo)

(define setup-geo
  (js-function->procedure
   "function(locationCallback) {
        return navigator.geolocation.watchPosition(
            function(evt) { locationCallback(plt.runtime.makeFloat(evt.latitude),
                                             plt.runtime.makeFloat(evt.longitude)); })}"))

(define shutdown-geo
  (js-function->procedure
   "function(watchId) {
        navigator.geolocation.clearWatch(watchId); }"))


;; The new event handler type for geolocation:
(define on-geo (make-world-event-handler setup-geo shutdown-geo))


