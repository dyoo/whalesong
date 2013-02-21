#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))
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
           (cond [(view-focus? view name)
                  (define focused (view-focus view name))
                  (cond
                   [(member name w)
                    view]
                   [else
                    (view-remove focused)])]
                 [else
                  view]))
         dom-view
         dwarf-names))



;; The first view consists of index.html.  We attach event handlers
;; to each name here.
(define my-view
  (view-bind-many* (->view index.html)
                   (map (lambda (name)
                          (list name "click" hide-on-click))
                        dwarf-names)))


(big-bang dwarf-names
          (initial-view my-view)
          (to-draw draw))
