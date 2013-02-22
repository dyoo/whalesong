#lang whalesong
(require whalesong/web-world
         whalesong/resource)

(define-resource index1.html)
(define-resource index2.html)


;; draw: world view -> view
(define (draw w v)
  (cond
   [(= (modulo w 2) 0)
    index1.html]
   [else
    index2.html]))


;; tick: world view -> world
(define (tick w v)
  (+ w 1))


(printf "Before the big bang\n")
(big-bang 0
          (initial-view index1.html)
          (to-draw draw)
          (on-tick tick 1)
          (stop-when (lambda (w v)
                       (> w 10))))
(printf "After the big bang\n")
