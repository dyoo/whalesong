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



(define (my-view-top v)
  (cond [(view-up? v)
         (my-view-top (view-up v))]
        [else
         v]))


;; Trying attribute editing
(view-attr (view-down
            (view-right
             (view-down
              (->view (xexp->dom `(html (head)
                                        (body (p (@ (class "blah"))))))))))
           "class")

(view-attr (update-view-attr (view-down
                     (view-right
                      (view-down
                       (->view (xexp->dom `(html (head)
                                                 (body (p (@ (class "blah"))))))))))
                    "class"
                    "baz")
           "class")

(view->xexp
 (my-view-top
  (update-view-attr (view-down
                     (view-right
                      (view-down
                       (->view (xexp->dom `(html (head)
                                                 (body (p (@ (class "blah"))))))))))
                    "class"
                    "baz")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
"css test"

(view-css (view-down
            (view-right
             (view-down
              (->view (xexp->dom `(html (head)
                                        (body (p (@ (style "text-decoration: line-through"))))))))))
           "text-decoration")

(view-css (update-view-css (view-down
                     (view-right
                      (view-down
                       (->view (xexp->dom `(html (head)
                                                 (body (p (@ (style "text-decoration: line-through"))))))))))
                    "text-decoration"
                    "underline")
           "text-decoration")

(view->xexp
 (my-view-top
  (update-view-css (view-down
                     (view-right
                      (view-down
                       (->view (xexp->dom `(html (head)
                                                 (body (p (@ (style "text-decoration: line-through"))))))))))
                    "text-decoration"
                    "underline")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)
"------"
"navigation"
(view-down? (->view (xexp->dom `(html))))
(view-up? (->view (xexp->dom `(html))))
(view-left? (->view (xexp->dom `(html))))
(view-right? (->view (xexp->dom `(html))))

(newline)

(view-down? (->view (xexp->dom `(html (head) (body)))))
(view-up? (->view (xexp->dom `(html (head) (body)))))
(view-left? (->view (xexp->dom `(html (head) (body)))))
(view-right? (->view (xexp->dom `(html (head) (body)))))

(newline)

(view-down? (view-down (->view (xexp->dom `(html (head) (body))))))
(view-up? (view-down (->view (xexp->dom `(html (head) (body))))))
(view-left? (view-down (->view (xexp->dom `(html (head) (body))))))
(view-right? (view-down (->view (xexp->dom `(html (head) (body))))))

(newline)

(view-down? (view-right (view-down (->view (xexp->dom `(html (head) (body)))))))
(view-up? (view-right (view-down (->view (xexp->dom `(html (head) (body)))))))
(view-left? (view-right (view-down (->view (xexp->dom `(html (head) (body)))))))
(view-right? (view-right (view-down (->view (xexp->dom `(html (head) (body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)
"------"
"adding elements"
(view->xexp
 (view-append-child (view-focus (->view (xexp->dom '(html (head)
                                                          (body (h1 (@ (id "header")))
                                                                (p (@ (id "para")))))))
                                "para")
                    (xexp->dom '(li "An item"))))


(view->xexp
 (view-insert-right
  (view-down
   (view-down
    (view-focus (->view (xexp->dom '(html (head)
                                          (body (h1 (@ (id "header")))
                                                (p (@ (id "para"))
                                                   (ul (li "one")))))))
                "para")))
  (xexp->dom '(li "two"))))


(view->xexp
 (view-insert-right
  (view-insert-right
   (view-down
    (view-down (view-focus (->view (xexp->dom '(html (head)
                                                     (body (h1 (@ (id "header")))
                                                           (p (@ (id "para"))
                                                              (ul (li "one")))))))
                           "para")))
   (xexp->dom '(li "two")))
  (xexp->dom '(li "three"))))

(view->xexp
 (view-insert-left
  (view-down
   (view-down (view-focus (->view (xexp->dom '(html (head)
                                                    (body (h1 (@ (id "header")))
                                                          (p (@ (id "para"))
                                                             (ul (li "one")))))))
                          "para")))
  (xexp->dom '(li "zero"))))
