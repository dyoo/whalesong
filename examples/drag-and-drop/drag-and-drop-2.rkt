#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource view.html)
(define-resource style.css)

;; A small drag-and-drop example using the web-world library.
;;
;; The world consists of a set of boxes.  It also has a reference
;; to the currently dragged box, if one is being dragged.
(define-struct world (boxes  ;; (listof box)
                      dragged ;; (U box #f)
                      ))

;; A box has an id and a position.
(define-struct box (id x y))



;; add-fresh-box: world view -> world
;; Given a world, creates a new world within the boundaries of the playground.
(define (add-fresh-box w v)
  (define-values (max-width max-height) (width-and-height "playground"))
  (define new-world (make-world (cons (make-box (fresh-id)
                                                (random max-width)
                                                (random max-height))
                                      (world-boxes w))
                                (world-dragged w)))
  new-world)



;; FIXME: do some javascript stuff here to get at this.
;;
(define (width-and-height element-id)
  (values 500 500))


(define (draw w v)
  (foldl (lambda (a-box v)
           (cond
            [(view-focus? v (box-id a-box))
             v]
            [else
             (view-append-child v
                                (xexp->dom `(span (@ (class "box")
                                                    (id ,(box-id a-box))
                                                    (style ,(format "position: absolute; left: ~apx; top: ~apx"
                                                                   (box-x a-box)
                                                                   (box-y a-box))))
                                                 "box")))]))
         (view-focus v "playground")
         (world-boxes w)))


;; When the mouse is down, we see if the event intersects any of our boxes.
(define (mousedown w v evt)
  ...)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-view (view-bind-many view.html
                                 ["add" "click" add-fresh-box]
                                 ["playground" "mousedown" mousedown]
                                 ["playground" "mousemove" mousemove]
                                 ["playground" "mouseup" mouseup]))
  
(big-bang (make-world (list) #f)
          (initial-view the-view)
          (to-draw draw))
          