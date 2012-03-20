#lang planet dyoo/whalesong

;; A simple binding to Google Maps.
;;
;; Some of this comes from:
;;
;;     https://developers.google.com/maps/documentation/javascript/tutorial
;; 

(require (planet dyoo/whalesong/js)
         (planet dyoo/whalesong/js/world)
         (planet dyoo/whalesong/web-world))


;; initialize-google-maps-api!: string boolean -> void
;; Dynamically loads the Google Maps API.
(define raw-initialize-google-maps-api!
  (js-async-function->procedure
   #<<EOF
function(success, fail, key, sensor) {
    var callbackName = 'afterGoogleMapsInitialized' + plt.runtime.makeRandomNonce();
    window[callbackName] = function() {
        delete(window[callbackName]);
        // At this point, we know the API has been instantiated ok.
        success(plt.runtime.VOID);
    };

    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src = "http://maps.googleapis.com/maps/api/js?key="
         + encodeURIComponent(key) + "&sensor=" + (sensor ? 'true' : 'false')
         + "&callback=" + encodeURIComponent(callbackName);
    document.body.appendChild(script);
}
EOF
))


;; raw-make-map-dom-and-map: js-number js-number -> (values dom-node gmap-object)
;; Dynamically creates both a dom-node and a gmap object.
(define raw-make-map-dom-and-map
  (js-async-function->procedure
   #<<EOF
function(success, fail, lat, lng) {
    var myOptions = {
         center: new google.maps.LatLng(lat, lng),
         zoom: 8,
         mapTypeId: google.maps.MapTypeId.ROADMAP
    };
    var domElement = document.createElement('div');
    domElement.style.width = "100%";
    domElement.style.height = "200px";
    var map = new google.maps.Map(domElement, myOptions);
    success(domElement, map);
}
EOF
))


;; We can listen to certain events, like click.
;; https://developers.google.com/maps/documentation/javascript/events
(define (make-on-map-click a-gmap)
  ;; setup will add a listener associated to the given map.
  (define raw-setup
    (js-function->procedure #<<EOF
function(map, callback) {
    var mapsListener =
        google.maps.event.addListener(map, 'click', function(event) {
            callback(plt.runtime.makeFloat(event.latLng.lat()),
                     plt.runtime.makeFloat(event.latLng.lng()));
        });
   return mapsListener;
}
EOF
))
  ;; shutdown will remove the listener off the map.
  (define raw-shutdown
    (js-function->procedure #<<EOF
function(gmap, mapsListener) {
    google.maps.event.removeListener(gmap, mapsListener);
}
EOF

))

  (define (setup callback)
    (raw-setup a-gmap callback))

  (define (shutdown setup-data)
    (raw-shutdown a-gmap setup-data))
  
  (make-world-event-handler setup shutdown))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-google-maps-api! key sensor)
  (unless (string? key)
    (raise-type-error 'initialize-google-maps-api! "string" 0 key))
  (unless (boolean? sensor)
    (raise-type-error 'initialize-google-maps-api! "boolean" 1 sensor))
  (raw-initialize-google-maps-api! key sensor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; This is dyoo's API key.
(printf "Loading google maps api\n");
(initialize-google-maps-api! "AIzaSyCRKQNI_nbyyN1286cssEy3taKj5IZcHN8" #f)
(printf "google maps api loaded\n")


;; We dynamically create a dom node for the presentation of the map,
;; and an auxiliary gmap value that we use to manage the internal
;; state of the map.
(define-values (dom gmap)
  (raw-make-map-dom-and-map (number->js-number -34.397)
                            (number->js-number 150.644)))



;; on-map-click: world handler
;; Creates an on-map-click associated to the gmap, ready to be used in
;; a big bang.
;; It'll be used as an input device for our world program.
(define on-map-click (make-on-map-click gmap))

dom



(big-bang 'nothing
          (on-map-click (lambda (w v lat lng)
                          (list lat lng))))