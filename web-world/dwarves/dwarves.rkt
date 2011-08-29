#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))
(define-resource index.html)

;; make-item: string -> view
(define (make-item name)
  (view-bind (->view `(li ,name))
             "click"
             hide-on-click))


;; When a dwarf clicks, it hides!
(define (hide-on-click w v)
  (view-hide v))


(define dwarf-names 
  '("Doc" "Grumpy" "Happy" "Sleepy" "Bashful" "Sneezy" "Dopey"))


;; Update the view so it shows the next dwarf on the scene,
;; until we're all done.
(define (draw w v)
  (cond [(< w (length dwarf-names))
         (view-append-child (view-focus v "#list")
                            (make-item (list-ref dwarf-names w)))]
        [else
         v]))


;; tick: world view -> world
(define (tick w v)
  (add1 w))


(big-bang 0
          (initial-view index.html)
          (on-tick tick .5)
          (to-draw draw))
