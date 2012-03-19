#lang planet dyoo/whalesong

;; A simple binding to Google Maps.

(require (planet dyoo/whalesong/js))


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
        success();
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-google-maps-api! key sensor)
  (unless (string? key)
    (raise-type-error 'initialize-google-maps-api! "string" 0 key))
  (unless (boolean? sensor)
    (raise-type-error 'initialize-google-maps-api! "boolean" 1 sensor))
  (void (raw-initialize-google-maps-api! key sensor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is dyoo's API key.
(printf "Loading google maps api\n");
(initialize-google-maps-api! "AIzaSyCRKQNI_nbyyN1286cssEy3taKj5IZcHN8" #f)
(printf "google maps api loaded\n")



(raw-make-map-dom-and-map (number->js-number -34.397)
                          (number->js-number 150.644))

"done"