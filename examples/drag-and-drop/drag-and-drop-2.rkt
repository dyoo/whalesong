#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

(define-resource view.html)
(define-resource style.css)

;; A small drag-and-drop example using the web-world library.
;;
;; The world consists of a set of shapes.  It also has a reference
;; to the currently dragged shape, if one is being dragged.
(define-struct world (shapes  ;; (listof shape)
                      dragged ;; (U shape #f)
                      ))

;; A shape has an id and a position.
(define-struct shape (id x y))



;; add-fresh-shape: world view -> world
;; Given a world, creates a new world within the boundaries of the playground.
(define (add-fresh-shape w v)
  (define-values (max-width max-height) (width-and-height v "playground"))
  (define new-world (make-world (cons (make-shape (fresh-id)
                                                (random max-width)
                                                (random max-height))
                                      (world-shapes w))
                                (world-dragged w)))
  new-world)



(define (width-and-height v element-id)
  (define focused (view-focus v element-id))
  (values (view-width focused)
          (view-height focused)))


(define (draw w v)
  (foldl (lambda (a-shape v)
           (cond
            [(view-focus? v (shape-id a-shape))
             (define focused (view-focus v (shape-id a-shape)))
             (update-view-css (update-view-css focused "left" (format "~apx" (shape-x a-shape)))
                              "top"
                              (format "~apx" (shape-y a-shape)))]
            [else
             (view-bind-many
              (view-append-child v
                                 (xexp->dom `(span (@ (class "shape")
                                                      (id ,(shape-id a-shape))
                                                      (style ,(format "position: absolute; left: ~apx; top: ~apx"
                                                                      (shape-x a-shape)
                                                                      (shape-y a-shape))))
                                                   "shape")))
              [(shape-id a-shape) "mousedown" mousedown])]))
         (view-focus v "playground")
         (if (shape? (world-dragged w))
             (cons (world-dragged w) (world-shapes w))
             (world-shapes w))))


;; find-shape: (listof shape) string -> (U #f shape)
(define (find-shape los id)
  (cond
   [(empty? los)
    #f]
   [(string=? (shape-id (first los)) id)
    (first los)]
   [else
    (find-shape (rest los) id)]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse handling.

;; When the mouse is down, select the shape being clicked.
(define (mousedown w v evt)
  (define selected-shape (find-shape (world-shapes w) (view-id v)))
  (make-world (remove selected-shape (world-shapes w))
              selected-shape))



(define (mouseup w v evt)
  (cond [(shape? (world-dragged w))
         (make-world (cons (world-dragged w)
                           (world-shapes w))
                     #f)]
        [else
         w]))

(define (mousemove w v evt)
  (cond
   [(shape? (world-dragged w))
    (define-values (left top) (normalize-mouse-event-coordinates v evt))
    (make-world (world-shapes w)
                (make-shape (shape-id (world-dragged w))
                            left
                            top))]
  [else
   w]))

(define (normalize-mouse-event-coordinates v evt)
  (values (- (event-ref evt "pageX")
             (string->number (trim-px (view-css v "left"))))
          (- (event-ref evt "pageY")
             (string->number (trim-px (view-css v "top"))))))

(define (trim-px s)
  (substring s 0 (- (string-length s) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-view (view-bind-many view.html
                                 ["add" "click" add-fresh-shape]
                                 ["playground" "mousemove" mousemove]
                                 ["playground" "mouseup" mouseup]))
  
(big-bang (make-world (list) #f)
          (initial-view the-view)
          (to-draw draw))
          