#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/web-world))

;; tick: world view -> world
(define (tick world view)
  (add1 world))

;; draw: world view -> view
(define (draw world view)
  (view-append-child view
                     (xexp->dom `(p "hello, can you see this? "
                                    ,(number->string world)))))

(big-bang 0 (initial-view
             (xexp->dom '(html (head) (body))))
            (on-tick tick 1)
            (to-draw draw))
