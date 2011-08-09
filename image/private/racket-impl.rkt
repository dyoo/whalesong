#lang s-exp "../../lang/base.rkt"

(require 2htdp/image
         (for-syntax racket/base))

(provide text
         text/font
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
         empty-scene
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


         image?
         ;; Something funky is happening on the Racket side of things with regards
         ;; to step-count?  See:  http://bugs.racket-lang.org/query/?cmd=view&pr=12031
         ;; step-count?

         bitmap/url
         )



(define-syntax (define-stubs stx)
  (syntax-case stx ()
    [(_ f ...)
     (syntax/loc stx
       (begin
         (define f (lambda args (error 'f))) ...))]))



(define-stubs color-list->image)


  

(define (my-step-count? x)
  (and (integer? x)
       (>= x 1)))


(define (png-bytes->image bytes)
  (error 'png-bytes->image "not implemented yet"))


(define image-url (procedure-rename bitmap/url 'image-url))
(define open-image-url (procedure-rename bitmap/url 'open-image-url))


(provide (rename-out [my-step-count? step-count?]
                     [bitmap/url image-url]
                     [bitmap/url open-image-url]))