#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)

(define-struct coord (lat lng))
(define-struct world (real mock))



(define (location-change world dom evt)
  (make-world (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))
              (world-mock world)))


(define (mock-location-change world dom evt)
  (make-world (world-real world)
              (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))))


(define (draw world dom)
  (define v1 (update-view-text (view-focus dom "#real-location")
                               (format "lat=~a, lng=~a"
                                       (coord-lat (world-real world))
                                       (coord-lng (world-real world)))))
  (define v2 (update-view-text (view-focus v1 "#mock-location")
                               (format "lat=~a, lng=~a"
                                       (coord-lat (world-mock world))
                                       (coord-lng (world-mock world)))))
  v2)



(big-bang (make-world 'unknown 'unknown)
          (initial-view index.html)
          (to-draw draw)
          (on-location-change location-change)
          (on-mock-location-change mock-location-change))