#lang s-exp "../../lang/base.rkt"

(require (for-syntax racket/base))

(provide text
         text/font
         overlay
         overlay/offset
         overlay/xy
         overlay/align
         underlay
         underlay/offset
         underlay/xy
         underlay/align
         beside
         beside/align
         above
         above/align
         empty-scene
         put-image
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
         polygon
         regular-polygon
         ellipse
         triangle
         triangle/sas
         triangle/sss
         triangle/ass
         triangle/ssa
         triangle/aas
         triangle/asa
         triangle/saa
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
         image?
         image=?
         ;; Something funky is happening on the Racket side of things with regards
         ;; to step-count?  See:  http://bugs.racket-lang.org/query/?cmd=view&pr=12031
         ;; step-count?
         
         bitmap/url
         video/url
         play-sound
         
         name->color
         
         step-count?
         image-url
         open-image-url
         color-list->bitmap
         
         )


;(provide (rename-out (my-color-list->bitmap color-list->bitmap)))

;(define (my-color-list->bitmap x w h)
;  (color-list->bitmap x w h))
;(set! my-color-list->bitmap my-color-list->bitmap)


(define-syntax (define-stubs stx)
  (syntax-case stx ()
    [(_ f ...)
     (syntax/loc stx
       (begin
         (begin (define f (lambda args (error 'f "stub definition")))
                (set! f f))
         ...))]))



(define-stubs 
  text
  text/font
  overlay
  overlay/offset
  overlay/xy
  overlay/align
  underlay
  underlay/offset
  underlay/xy
  underlay/align
  beside
  beside/align
  above
  above/align
  empty-scene
  put-image
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
  polygon
  regular-polygon
  ellipse
  triangle
  triangle/sas
  triangle/sss
  triangle/ass
  triangle/ssa
  triangle/aas
  triangle/asa
  triangle/saa
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
  
  image?
  image=?
  ;; Something funky is happening on the Racket side of things with regards
  ;; to step-count?  See:  http://bugs.racket-lang.org/query/?cmd=view&pr=12031
  ;; step-count?
  bitmap/url
  video/url
  play-sound
  name->color
  step-count?
  image-url
  open-image-url
  color-list->bitmap
  )





;(define (my-step-count? x)
;  (and (integer? x)
;       (>= x 1)))


;(define image-url (procedure-rename bitmap/url 'image-url))
;(define open-image-url (procedure-rename bitmap/url 'open-image-url))


#;(define (name->color n)
    (error 'name->color "not implemented yet"))


#;(provide (rename-out [my-step-count? step-count?]
                       [bitmap/url image-url]
                       [bitmap/url open-image-url]))
