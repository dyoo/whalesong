#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)

(define-struct coord (lat lng))
(define-struct world (real mock))



(define (location-change world dom evt)
  world)


(define (mock-location-change world dom evt)
  world)


(define (draw world dom)
  dom)


(big-bang (make-world 'unknown 'unknown)
          (initial-view index.html)
          (to-draw draw)
          (on-location-change location-change)
          (on-mock-location-change mock-location-change))