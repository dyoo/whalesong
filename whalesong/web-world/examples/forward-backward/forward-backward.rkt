#lang whalesong

(require whalesong/resource
         whalesong/web-world)

(define-resource index.html)

(define (go-forward world dom)
  (add1 world))


(define (go-backward world dom)
  (max (sub1 world) 0))


(define (view-top a-view)
  (if (view-up? a-view)
      (view-top (view-up a-view))
      a-view))


(define (clear-all a-view)
  (define (loop a-view n)
    (define updated-view (update-view-css a-view "background" ""))
    (cond
     [(view-forward? updated-view)
      (loop (view-forward updated-view) (add1 n))]
     [else
      (view-top updated-view)]))
  (loop a-view 0))
    

(define (iterate n f x)
  (if (<= n 0)
      x
      (iterate (sub1 n) f (f x))))


(define (maybe-view-forward a-view)
  (if (view-forward? a-view)
      (view-forward a-view)
      a-view))


(define (draw world dom)
  (define another-view (update-view-css (iterate world
                                                 maybe-view-forward
                                                 (clear-all dom))
                                        "background"
                                        "orange"))
  
  (update-view-text (view-focus another-view "message")
                    (format "Highlighting element ~a\n" world)))

(define my-initial-view (view-bind
                         (view-focus
                          (view-bind
                           (view-focus (->view index.html)
                                       "forward")
                           "click"
                           go-forward)
                          "backward")
                         "click"
                         go-backward))

(big-bang 0
          (initial-view my-initial-view)
          (to-draw draw))
