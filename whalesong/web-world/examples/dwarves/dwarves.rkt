#lang whalesong
(require whalesong/web-world
         whalesong/resource)
(define-resource index.html)

;; The world is the set of dwarfs.


;; make-item: string -> view
(define (make-item name)
  (view-bind (->view `(li ,name))
             "click"
             hide-on-click))


;; When a dwarf clicks, it hides!
(define (hide-on-click w v)
  (remove (view-id v) w))


(define dwarf-names 
  '("Doc" "Grumpy" "Happy" "Sleepy" "Bashful" "Sneezy" "Dopey"))


;; Update the view so it shows the next dwarf on the scene,
;; until we're all done.
(define (draw w dom-view)
  (foldl (lambda (name view)
           (define focused (view-focus view name))
           (cond
            [(member name w)
             (view-show focused)]
            [else
             (view-hide focused)]))
         dom-view
         dwarf-names))



;; The first view consists of index.html.  We attach event handlers
;; to each name here.
(define my-view
  (foldl (lambda (name view)
           (view-bind (view-focus view name)
                      "click"
                      hide-on-click))
         (->view index.html)
         dwarf-names))


(big-bang dwarf-names
          (initial-view my-view)
          (to-draw draw))
