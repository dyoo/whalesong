#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource view.html)
(define-resource style.css)

;; A small drag-and-drop example using the web-world library.
;;
;; The world consists of a set of shapes.
;;
;; A shape has an id and a position.
(define-struct shape (id x y))



;; add-fresh-shape: world view -> world
;; Given a world, creates a new world within the boundaries of the playground.
(define (add-fresh-shape w v)
  (define-values (max-width max-height) (width-and-height v "playground"))
  (define new-world (cons (make-shape (fresh-id)
                                      (random max-width)
                                      (random max-height))
                          w))
  new-world)



(define (width-and-height v element-id)
  (define focused (view-focus v element-id))
  (values (view-width focused)
          (view-height focused)))


(define (draw w v)
  (foldl (lambda (a-shape v)
           (cond
            [(view-focus? v (shape-id a-shape))
             v]
            [else
             (view-append-child v
                                (xexp->dom `(span (@ (class "shape")
                                                     (id ,(shape-id a-shape))
                                                     (style ,(format "position: absolute; left: ~apx; top: ~apx"
                                                                     (shape-x a-shape)
                                                                     (shape-y a-shape))))
                                                  "shape")))]))
         (view-focus v "playground")
         w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-view (view-bind-many view.html
                                 ["add" "click" add-fresh-shape]))

(big-bang (list)
          (initial-view the-view)
          (to-draw draw))
