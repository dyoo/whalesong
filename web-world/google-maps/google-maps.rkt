#lang s-exp "../../lang/base.rkt"

(require "../../js.rkt")

(provide load-gmaps-library)


(define (load-gmaps-library api-key with-sensor)
  (load-script (string-append
                "http://maps.googleapis.com/maps/api/js?key="
                api-key
                "&sensor="
                (if with-sensor "true" "false"))))