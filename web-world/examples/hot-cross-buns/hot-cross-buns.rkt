#lang planet dyoo/whalesong/cs019

(define-resource index.html)

(define base-view (->view index.html))

(define WORDS (list "hot" "cross" "buns"))


(define (remove-all x elts)
  (cond
   [(empty? elts)
    empty]
   [(equal? x (first elts))
    (remove-all x (rest elts))]
   [else
    (cons (first elts)
          (remove-all x (rest elts)))]))

(define view-with-buttons
  (foldl (lambda (name a-view)
           (view-bind (view-focus a-view name)
                      "click"
                      (lambda (world a-view)
                        (cond
                         [(view-has-attr? a-view "checked")
                          (cons name world)]
                         [else
                          (remove-all name world)]))))
         base-view
         WORDS))

(define view-with-buttons-and-reset
  (view-bind (view-focus view-with-buttons "reset")
             "click"
             (lambda (world a-view)
               empty)))


(define (draw world v)
  (local ([define view-with-updated-message
            (update-view-text (view-focus v "mydiv")
                              (format "~s" world))])
     (foldl (lambda (word a-view)
              (local [(define view-on-word
                        (view-focus a-view word))]
                 (cond
                  [(and (view-has-attr? view-on-word "checked")
                        (not (member word world)))
                   (remove-view-attr view-on-word "checked")]
                  [(and (not (view-has-attr? view-on-word "checked"))
                        (member word world))
                   (update-view-attr view-on-word "checked" "checked")]
                  [else
                   a-view])))
            view-with-updated-message
            WORDS)))

(big-bang '()
          (initial-view view-with-buttons-and-reset)
          (to-draw draw))