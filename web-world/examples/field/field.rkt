#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource index.html)

;; The world is a string which represents the name of the user.


;; on-click: world view -> world
;; When the user clicks on the button, grab at the text of the
;; text-field.
(define (on-click w button-view)
  (view-form-value (view-focus button-view "#text-field")))


;; draw: world view -> view
;; Take the view, and replace the template with the world value.
(define (draw w dom)
  (update-view-text (view-focus dom "#template")
                    w)) 


(define my-view (view-bind (view-focus (->view index.html) 
                                       "#button")
                           "click"
                           on-click))

(big-bang "Jane Doe"
          (initial-view my-view)
          (to-draw draw))