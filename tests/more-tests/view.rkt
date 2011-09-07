#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world))

(define view (->view (xexp->dom `(html (head)
                                       (body (p "hello world, this is a test")
                                             (div (@ (id "a div"))))))))
(define new-view
  (view-focus view "a div"))

(view-text new-view) ;; should be ""

(define updated-new-view
  (update-view-text new-view "some text"))

(view-text updated-new-view) ;; should be "some text"

(view->xexp (view-up (view-up updated-new-view)))


(view-attr (view-down
            (view-right
             (view-down
              (->view (xexp->dom `(html (head)
                                        (body (p (@ (class "blah"))))))))))
           "class")


(define (my-view-top v)
  (cond [(view-up? v)
         (my-view-top (view-up v))]
        [else
         v]))

(view->xexp
 (my-view-top
  (update-view-attr (view-down
                     (view-right
                      (view-down
                       (->view (xexp->dom `(html (head)
                                                 (body (p (@ (class "blah"))))))))))
                    "class"
                    "baz")))
