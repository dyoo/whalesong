#lang s-exp "../../lang/base.rkt"

;; Image functions that be implemented using racket based on primitives

(require "main.rkt"
         "../../lang/for.rkt"
         "../../lang/posn.rkt")

(provide place-images
         place-images/align)

; place-images : (listof image?) (listof posn?) image? -> image?
(define (place-images images posns scene)
  (for/fold ([acc scene])
            ([img images] [posn posns])
            (place-image img (posn-x posn) (posn-y posn) acc)))

; place-images : (listof image?) (listof posn?) x-place? y-place? image? -> image?
(define (place-images/align images posns x-place y-place scene)
  (for/fold ([acc scene])
            ([img images] [posn posns])
            (place-image/align img (posn-x posn) (posn-y posn) x-place y-place acc)))
