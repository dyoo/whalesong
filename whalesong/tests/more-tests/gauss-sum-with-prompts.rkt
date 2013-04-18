#lang whalesong

(define (f i acc)
  (cond [(> i 0)
         (abort-current-continuation (default-continuation-prompt-tag)
                                     (lambda ()
                                       (f (sub1 i) (+ i acc))))
         (printf "You should not see this\n")
         (/ 1 0)]
        [else
         acc]))
         
(define (gauss i)
  (call-with-continuation-prompt (lambda ()
                                   (f i 0))))

(gauss 100)
