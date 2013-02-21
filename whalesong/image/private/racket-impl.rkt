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

         name->color
         )


(provide (rename-out (my-color-list->bitmap color-list->bitmap)))

(define (my-color-list->bitmap x w h)
  (color-list->bitmap x w h))
(set! my-color-list->bitmap my-color-list->bitmap)


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


(define image-url (procedure-rename bitmap/url 'image-url))
(define open-image-url (procedure-rename bitmap/url 'open-image-url))


(define (name->color n)
  (error 'name->color "not implemented yet"))


(provide (rename-out [my-step-count? step-count?]
                     [bitmap/url image-url]
                     [bitmap/url open-image-url]))