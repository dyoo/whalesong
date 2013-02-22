#lang whalesong

(require whalesong/web-world
         whalesong/resource)

;; The world is a string, the current color.
(define-resource view.html)


;; update the world to the color indicated by the clicked button's
;; value.
(define (click w v)
  (view-form-value v))

(define bound-view
  (view-bind-many view.html
                  ["red-button" "click" click]
                  ["gray-button" "click" click]
                  ["blue-button" "click" click]
                  ["orange-button" "click" click]
                  ["green-button" "click" click]
                  ["black-button" "click" click]))

(define (draw w v)
  (define view-on-header (view-focus v "header"))
  (update-view-css view-on-header "color" w))


(big-bang "Black"
          (initial-view bound-view)
          (to-draw draw))
