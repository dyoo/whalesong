#lang s-exp "../../lang/base.rkt"

(require 2htdp/image
         (for-syntax racket/base))

(provide text
         text/font
         image-url
         open-image-url
         overlay
         overlay/xy
         overlay/align
         underlay
         underlay/xy
         underlay/align
         beside
         beside/align
         above
         above/align
         place-image
         place-image/align
         rotate
         scale
         scale/xy
         flip-horizontal
         flip-vertical
         frame
         crop
         line
         add-line
         scene+line
         circle
         square
         rectangle
         regular-polygon
         ellipse
         triangle
         right-triangle
         isosceles-triangle
         star
         radial-star
         star-polygon
         rhombus
         image->color-list
         color-list->image
         image-width
         image-height
         image-baseline
         image-color?
         mode?
         x-place?
         y-place?
         angle?
         side-count?
         image-color?

         ;; Something funky is happening on the Racket side of things with regards
         ;; to step-count?  See:  http://bugs.racket-lang.org/query/?cmd=view&pr=12031
         ;; step-count?
         )



(define-syntax (define-stubs stx)
  (syntax-case stx ()
    [(_ f ...)
     (syntax/loc stx
       (begin
         (define f (lambda args (error 'f))) ...))]))



(define-stubs
  image-url
  open-image-url
  color-list->image
           )



(define (my-step-count? x)
  (and (integer? x)
       (>= x 1)))

(provide (rename-out [my-step-count? step-count?]))