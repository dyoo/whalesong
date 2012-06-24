#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource view.html)
(define-resource style.css)

;; A small drag-and-drop example using the web-world library.
;;
;; The world consists of a set of boxes.
;;
;; A box has an id and a position.
(define-struct box (id x y))



;; add-fresh-box: world view -> world
;; Given a world, creates a new world within the boundaries of the playground.
(define (add-fresh-box w v)
  (define-values (max-width max-height) (width-and-height "playground"))
  (define new-world (cons (make-box (fresh-id)
                                    (random max-width)
                                    (random max-height))
                          w))
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
         w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-view (view-bind-many view.html
                                 ["add" "click" add-fresh-box]))
  
(big-bang (list)
          (initial-view the-view)
          (to-draw draw))
          