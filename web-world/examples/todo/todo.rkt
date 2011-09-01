#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

;; The world is our TODO list, represented as a list of strings.

(define-resource index.html)


;; An item consists of a string id, the item's content, and a finished? flag.
(define-struct item (id ;; string
                     content ;; string
                     finished? ;; boolean
                     ))


;; new-item: string -> item
(define (new-item content)
  (make-item (fresh-id) content #f))


;; toggle-item-finished: world string -> world
;; Mark the item with the given id so that it's finished, or reverse that change.
(define (toggle-item-finished world id)
  (cond
   [(empty? world)
    '()]
   [(string=? id (item-id (first world)))
    (cons (make-item id (item-content (first world)) (not (item-finished? (first world))))
          (rest world))]
   [else
    (cons (first world)
          (toggle-item-finished (rest world) id))]))



;; world view -> world
(define (on-add world view)
  (local [(define text (view-form-value (view-focus view "#next-item")))]
    (cons (new-item text) world)))


;; world view -> view
(define (draw world view)
  (foldl refresh-item-in-view
         view
         world))


;; refresh-item-in-view: item view -> view
(define (refresh-item-in-view item view)
  (cond
   [(view-focus? view (format "#~a" (item-id item)))
    (update-view-css (view-focus view (format "#~a" (item-id item)))
                     "text-decoration"
                     (cond [(item-finished? item)
                            "line-through"]
                           [else
                            "none"]))]
   [else
    (view-bind
     (view-append-child (view-focus view "#items")
                        (xexp->dom `(li (@ (id ,(item-id item)))
                                        ,(item-content item))))
     "click"
     when-item-clicked)]))



;; when-item-clicked: world view -> world
;; When an item is clicked, set its finished? flag.
(define (when-item-clicked world view)
  (toggle-item-finished world (view-attr view "id")))
  

(define the-view
  (view-bind (view-focus (->view index.html) "#add-button")
             "click"
             on-add))


(big-bang (list (new-item "milk")
                (new-item "eggs"))
          (initial-view the-view)
          (to-draw draw))