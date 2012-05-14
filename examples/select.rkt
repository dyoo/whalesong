#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world))

(define (draw w v)
  (define v2 (view-focus v "fill-me-in"))
  (update-view-text v2 w))

(define view (xexp->dom '(div
                          (h1 "test")
                          (select (@ (id "my-select"))
                                  (option (@ (value "red")) "Red")
                                  (option (@ (value "green")) "Green")
                                  (option (@ (value "blue")) "Blue"))
                          (p
                            "I see: "
                            (span (@ (id "fill-me-in")))))))

(define (when-select-changed w v)
  (view-form-value (view-focus v "my-select")))
   
(define bound-view (view-bind-many view ["my-select" "change" when-select-changed]))

(big-bang "nothing yet"
          (initial-view bound-view)
          (to-draw draw))
