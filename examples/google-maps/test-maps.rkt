#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         "maps.rkt")

;; This is dyoo's API key.
(printf "Loading google maps api\n");
(initialize-google-maps-api! "AIzaSyCRKQNI_nbyyN1286cssEy3taKj5IZcHN8" #f)
(printf "google maps api loaded\n")

;; We dynamically create a dom node for the presentation of the map,
;; and an auxiliary gmap value that we use to manage the internal
;; state of the map.
(define-values (dom gmap)
  (make-dom-and-map 41.82706261971936 -71.39962630844116))

;; on-map-click: world handler
;; Creates an on-map-click associated to the gmap, ready to be used in
;; a big bang.
;; It'll be used as an input device for our world program.
(define on-map-click (make-on-map-click gmap))



(big-bang "???"
          (initial-view
           (xexp->dom
            `(div (p (@ (id "where"))
                     "<<fill me in>>")
                  (hr)
                  ,dom
                  (hr)
                  (p "Instructions: click the map.  The "
                     "world program will follow the map clicks."))))
          (to-draw (lambda (w v)
                     (update-view-text (view-focus v "where")
                                       (format "~a" w))))
          (on-map-click (lambda (w v lat lng)
                          (list lat lng))))