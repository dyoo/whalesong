#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world)
         (planet dyoo/whalesong/resource))

;; The world is our TODO list, represented as a list of strings.

(define-resource index.html)

(define-struct item (id content))

(define (new-item content)
  (make-item (symbol->string (gensym 'item))
             content))



(define (on-add world view)
  (local [(define text (view-form-value (view-focus view "#next-item")))]
    (cons (new-item text) world)))



(define (draw world view)
  (foldl add-item-to-view
         view
         world))


(define (add-item-to-view item view)
  (cond
   [(view-focus? view (format "#~a" (item-id item)))
    view]
   [else
    (view-append-child (view-focus view "#items")
                       ;; FIXME: I want this to add a DOM to this.
                       (xexp->dom `(li (@ (id ,(item-id item)))
                                       ,(item-content item))))]))

  

(define the-view
  (view-bind (view-focus (->view index.html) "#add-button")
             "click"
             on-add))


(big-bang '()
          (initial-view the-view)
          (to-draw draw))