#lang whalesong/cs019

(define-resource index.html)

(define-struct: coord ([lat : Number$]
                       [lng : Number$]))

;; coord/unknown?: any -> boolean
;; Returns true if x is a coord or the symbol 'unknown.
(define (coord/unknown? x)
  (or (coord? x)
      (and (symbol? x)
           (symbol=? x 'unknown))))

(define Coord/Unknown$ (Sig: coord/unknown?))
  

;; The world stores both the real location, as well as a mocked-up
;; one.
(define-struct: world ([real : Coord/Unknown$]
                       [mock : Coord/Unknown$]))


(define: (location-change [world : world$] [dom : View$] [evt : Event$]) -> world$
  (make-world (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))
              (world-mock world)))


(define: (mock-location-change [world : world$] [dom : View$] [evt : Event$]) -> world$
  (make-world (world-real world)
              (make-coord (event-ref evt "latitude")
                          (event-ref evt "longitude"))))


(define: (draw [world : world$] [dom : View$]) -> View$
  (local [(define v1 (if (coord? (world-real world))
                         (update-view-text (view-focus dom "real-location")
                                           (format "lat=~a, lng=~a"
                                                   (coord-lat (world-real world))
                                                   (coord-lng (world-real world))))
                         dom))
          (define v2 (if (coord? (world-mock world))
                         (update-view-text (view-focus v1 "mock-location")
                                           (format "lat=~a, lng=~a"
                                                   (coord-lat (world-mock world))
                                                   (coord-lng (world-mock world))))
                         v1))]
         v2))


(big-bang (make-world 'unknown 'unknown)
          (initial-view index.html)
          (to-draw draw)
          (on-location-change location-change)
          (on-mock-location-change mock-location-change))
