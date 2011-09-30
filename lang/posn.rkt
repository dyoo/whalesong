#lang s-exp "kernel.rkt"

;; The posn struct for the teaching languages
(provide struct:posn make-posn posn? posn-x posn-y set-posn-x! set-posn-y!
         posn #;(rename-out (posn posn-id)))

(struct posn (x y) #:mutable #:transparent)

;; We define a separate function so tha it has the 
;; name `make-posn':
(define (make-posn x y) (posn x y))
