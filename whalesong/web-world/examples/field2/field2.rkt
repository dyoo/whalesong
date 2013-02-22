#lang whalesong
(require whalesong/web-world
         whalesong/resource)

(define-resource index.html)

;; The world is:
(define-struct world (name
                      elapsed))


;; on-click: world view -> world
;; When the user clicks on the button, grab at the text of the
;; text-field.
(define (on-click w button-view)
  (make-world (view-form-value (view-focus button-view "text-field"))
              (world-elapsed w)))


(define (tick w view)
  (make-world (world-name w)
              (add1 (world-elapsed w))))


;; draw: world view -> view
;; Take the view, and replace the template with the world value.
(define (draw w dom)
  (update-view-text
   (view-focus (update-view-text (view-focus dom "template")
                                 (world-name w))
               "header")
   (if (= (world-elapsed w) 1)
       (format "~a second has elapsed" (world-elapsed w))
       (format "~a seconds have elapsed" (world-elapsed w)))))


(define my-view (view-bind (view-focus (->view index.html) 
                                       "button")
                           "click"
                           on-click))


(big-bang (make-world "Jane Doe" 0)
          (initial-view my-view)
          (to-draw draw)
          (on-tick tick 1))
